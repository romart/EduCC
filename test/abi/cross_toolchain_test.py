#!/usr/bin/env python3
"""riscv64 ABI agreement with the cross toolchain.

Every other suite in this tree compares EduCC to EduCC. `crossabi` compares
its two backends, which says nothing about whether either agrees with the
*platform*: riscv64 has one backend anyway, so a rule both halves of a call
get wrong is a rule nothing notices.

This asks a third party. Each fixture is a pair - `<name>.c` holds main and
the call sites, `<name>.partner.c` the functions - and each pair is built
three ways:

  reference   both halves by riscv64-linux-gnu-gcc
  caller      the caller by EduCC, the partner by gcc
  partner     the caller by gcc, the partner by EduCC

All three have to print the same thing. The two mixed builds put the
disagreement on opposite sides of the boundary, so a rule EduCC has wrong
shows up whichever end of the call it is written down at.

Linking is EduCC's own in every build but the reference, which is also the
widest thing that links gcc-built riscv64 objects.

Exits 2 - the ctest SKIP code - where there is no cross toolchain or no qemu.
"""

import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
DATA = os.path.join(ROOT, "test", "testData", "abi")

GCC = "riscv64-linux-gnu-gcc"
QEMU = "qemu-riscv64"
SYSROOT = "/usr/riscv64-linux-gnu"


def run(cmd, **kwargs):
    return subprocess.run(cmd, capture_output=True, text=True, **kwargs)


class Failure(Exception):
    pass


def build(out, parts, workdir):
    """parts is a list of (source, compiler) pairs; EduCC does the link."""
    objects = []

    for source, compiler in parts:
        obj = os.path.join(workdir, os.path.basename(source) + "." + compiler + ".o")

        if compiler == "gcc":
            cmd = [GCC, "-O1", "-c", source, "-o", obj]
        else:
            cmd = [EDUCC, "-march", "riscv64", "-c", source, "-o", obj]

        result = run(cmd)
        if result.returncode != 0:
            raise Failure("compiling %s with %s failed:\n%s%s"
                          % (source, compiler, result.stdout, result.stderr))
        objects.append(obj)

    if all(compiler == "gcc" for _, compiler in parts):
        link = [GCC, "-o", out] + objects
    else:
        link = [EDUCC, "-march", "riscv64", "-o", out] + objects

    result = run(link)
    if result.returncode != 0:
        raise Failure("linking %s failed:\n%s%s" % (out, result.stdout, result.stderr))

    return out


def execute(binary):
    result = run([QEMU, "-L", SYSROOT, binary])
    if result.returncode != 0:
        raise Failure("%s exited %d:\n%s%s"
                      % (os.path.basename(binary), result.returncode,
                         result.stdout, result.stderr))
    return result.stdout


def checkFixture(name, workdir):
    caller = os.path.join(DATA, name + ".c")
    partner = os.path.join(DATA, name + ".partner.c")

    builds = {
        "reference": [(caller, "gcc"), (partner, "gcc")],
        "caller": [(caller, "educc"), (partner, "gcc")],
        "partner": [(caller, "gcc"), (partner, "educc")],
    }

    outputs = {}
    for label, parts in builds.items():
        binary = build(os.path.join(workdir, name + "." + label), parts, workdir)
        outputs[label] = execute(binary)

    findings = []
    for label in ("caller", "partner"):
        if outputs[label] == outputs["reference"]:
            continue

        expected = outputs["reference"].splitlines()
        actual = outputs[label].splitlines()
        lines = ["%s: EduCC compiled the %s half and it disagrees with gcc"
                 % (name, label)]

        for idx in range(max(len(expected), len(actual))):
            want = expected[idx] if idx < len(expected) else "<nothing>"
            got = actual[idx] if idx < len(actual) else "<nothing>"
            if want != got:
                lines.append("    gcc:   %s" % want)
                lines.append("    educc: %s" % got)

        findings.append("\n".join(lines))

    return findings


def main():
    global EDUCC

    if len(sys.argv) != 2:
        print("usage: cross_toolchain_test.py <path to educc>")
        return 1

    EDUCC = os.path.abspath(sys.argv[1])

    for tool in (GCC, QEMU):
        if shutil.which(tool) is None:
            print("SKIP: %s is not installed" % tool)
            return 2

    if not os.path.isdir(SYSROOT):
        print("SKIP: no riscv64 sysroot at %s" % SYSROOT)
        return 2

    workdir = os.path.join(os.environ.get("TMPDIR", "/tmp"), "educc-cross-abi")
    shutil.rmtree(workdir, ignore_errors=True)
    os.makedirs(workdir)

    names = sorted(f[:-2] for f in os.listdir(DATA)
                   if f.endswith(".c") and not f.endswith(".partner.c"))

    findings = []
    for name in names:
        try:
            findings += checkFixture(name, workdir)
            print("%s -- OK" % name)
        except Failure as failure:
            findings.append("%s: %s" % (name, failure))
            print("%s -- FAIL" % name)

    print("\n%d fixture(s), %d finding(s)" % (len(names), len(findings)))
    for finding in findings:
        print(finding)

    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
