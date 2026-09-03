#!/usr/bin/env python3
"""Build the Tiny C Compiler with EduCC, run tcc's own test suite, and measure it.

    python3 test/bench/bench_tcc.py                  # clone, build, test, bench
    python3 test/bench/bench_tcc.py --no-tests       # skip tcc's suite (~4 min)
    python3 test/bench/bench_tcc.py --only chaitin   # one configuration
    python3 test/bench/bench_tcc.py --keep           # do not re-clone or re-build

bench.py asks how good the code EduCC generates is over eight programs written
for the question. This asks the same of a program nobody wrote for it: tinycc
is 50k lines of other people's C, with its own test suite and its own opinion
about the SysV ABI, and it is small enough to build in seconds and self-hosting
enough to be its own workload.

Three things come out of it, and only the last is a benchmark:

  * whether tinycc builds at all under each configuration, and whether the tcc
    that comes out passes tinycc's suite - which is a real test suite for a
    real compiler, run against a binary EduCC produced
  * whether the object files those tccs emit are identical. tcc is
    deterministic, so a difference is a miscompilation of tcc by whichever
    configuration disagrees, and it names the file
  * how fast that tcc compiles, and how big it is

The reference columns are the host compiler at -O0/-O1/-O2 and tinycc compiled
by itself. The tcc column matters more than it looks: it is the control for
everything that is a property of building tinycc with something that is not
gcc, rather than of EduCC - GCC_MAJOR in config.h, say, which gates a block of
tcctest.c's output.

Not wired into ctest: it clones from the network, takes minutes, and times
things.
"""
import argparse
import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))

TCC_URL = "https://github.com/TinyCC/tinycc.git"
TCC_BRANCH = "dev"

# gcc-O2 before tcc: the tcc column is tinycc compiled by the tinycc gcc built.
CONFIGS = ["legacy", "trivial", "linear", "chaitin", "gcc-O0", "gcc-O1",
           "gcc-O2", "tcc"]

EDUCC_FLAG = {"legacy": "-legacy", "trivial": "-Xregalloc=trivial",
              "linear": "-Xregalloc=linear", "chaitin": ""}

# tinycc's own sources, as the "tcc compiling tcc" workload. ONE_SOURCE=0, so
# these are the ten translation units its Makefile builds.
TCC_SOURCES = ["tcc.c", "libtcc.c", "tccpp.c", "tccgen.c", "tccelf.c",
               "tccasm.c", "tccrun.c", "x86_64-gen.c", "x86_64-link.c",
               "i386-asm.c"]

# The test targets, spelled out rather than via `make test`: that target's first
# act is `make clean`, which deletes the reference output prepared below.
TCC_TESTS = ["hello-exe", "hello-run", "libtest", "libtest_mt", "test3",
             "memtest", "dlltest", "abitest", "asm-c-connect-test",
             "vla_test-run", "cross-test", "tests2-dir", "pp-dir", "btest",
             "test1b"]


def run(cmd, cwd=None, env=None, log=None):
    with open(log, "wb") if log else open(os.devnull, "wb") as sink:
        return subprocess.run(cmd, cwd=cwd, env=env, stdout=sink,
                              stderr=subprocess.STDOUT).returncode


def note(text):
    print("  " + text, flush=True)


def obtainSource(work, keep):
    """The tinycc checkout. Under the work directory, never beside the repo."""
    src = os.path.join(work, "tinycc")
    if os.path.isdir(os.path.join(src, ".git")):
        if not keep:
            run(["git", "-C", src, "fetch", "--depth", "50", "origin", TCC_BRANCH])
            run(["git", "-C", src, "reset", "--hard", "FETCH_HEAD"])
        return src
    os.makedirs(work, exist_ok=True)
    print("cloning tinycc (%s) into %s" % (TCC_BRANCH, src), flush=True)
    if run(["git", "clone", "--depth", "50", "-b", TCC_BRANCH, TCC_URL, src]) != 0:
        sys.exit("clone failed")
    return src


def writeWrappers(work, compiler, gccTcc, src):
    """One wrapper script per configuration, so the flag reaches tinycc's link
    step as well as its compile step - configure takes a command, not a flag."""
    d = os.path.join(work, "cc")
    os.makedirs(d, exist_ok=True)
    for name, flag in EDUCC_FLAG.items():
        p = os.path.join(d, "educc-" + name)
        with open(p, "w") as f:
            f.write('#!/bin/sh\nexec %s %s "$@"\n' % (compiler, flag))
        os.chmod(p, 0o755)
    p = os.path.join(d, "tcc-host")
    with open(p, "w") as f:
        f.write('#!/bin/sh\nexec %s -B%s -I%s/include "$@"\n'
                % (os.path.join(gccTcc, "tcc"), gccTcc, src))
    os.chmod(p, 0o755)
    return d


def buildOne(name, work, ccdir, src, reference):
    d = os.path.join(work, name)
    shutil.rmtree(d, ignore_errors=True)
    os.makedirs(d)

    env = dict(os.environ)
    extra = []
    if name.startswith("gcc-"):
        cc, env["CFLAGS"] = reference, "-Wall -" + name.split("-")[1]
    elif name == "tcc":
        cc, env["CFLAGS"] = os.path.join(ccdir, "tcc-host"), "-Wall -O2"
        # configure would otherwise use "$cc -ar", and tcc wants -ar as its very
        # first argument, which a wrapper carrying -B and -I cannot give it.
        extra = ["--ar=ar"]
    else:
        cc, env["CFLAGS"] = os.path.join(ccdir, "educc-" + name), "-Wall -O2"

    if run([os.path.join(src, "configure"), "--cc=" + cc] + extra,
           cwd=d, env=env, log=os.path.join(d, "configure.log")) != 0:
        return None, "configure failed"
    if run(["make", "-j%d" % (os.cpu_count() or 1)],
           cwd=d, env=env, log=os.path.join(d, "build.log")) != 0:
        return None, "build failed, see " + os.path.join(d, "build.log")
    return d, None


def reference(build, src, out):
    """tcctest.c's expected output, produced by the host compiler against *this
    build's* config.h. Per build, because tcctest.c reads GCC_MAJOR out of it and
    configure writes 0 there for every compiler it does not recognise as gcc; a
    shared reference reports that as a miscompilation.

    -fpermissive: tcctest.c is full of K&R definitions and implicit
    declarations, which gcc 14 and later reject outright rather than warn about.
    """
    exe = os.path.join(build, "tests", "tcctest.host")
    cmd = ["cc", "-o", exe, os.path.join(src, "tests", "tcctest.c"),
           "-I" + src, "-I" + build, "-DTCC_TARGET_X86_64", "-fpermissive",
           "-w", "-O0", "-std=gnu99", "-fno-omit-frame-pointer"]
    if run(cmd) != 0:
        return False
    with open(out, "wb") as f:
        return subprocess.run([exe], stdout=f).returncode == 0


def runTests(name, build, ccdir, src, reference_cc):
    tests = os.path.join(build, "tests")
    run(["make", "-C", tests, "-s", "clean"])
    if not reference(build, src, os.path.join(tests, "test.ref")):
        return None, "could not build the reference output"
    os.utime(os.path.join(tests, "test.ref"))

    if name.startswith("gcc-"):
        cc = reference_cc + " -fpermissive"
    elif name == "tcc":
        cc = os.path.join(ccdir, "tcc-host")
    else:
        cc = os.path.join(ccdir, "educc-" + name)

    log = os.path.join(build, "tests.log")
    run(["make", "-C", "tests", "-k", "-r"] + TCC_TESTS + ["CC=" + cc],
        cwd=build, log=log)

    # One section per "------------ name ------------" banner; a section that
    # make reported an error inside of is a failure.
    sections, failed, current = [], set(), None
    for line in open(log, errors="replace"):
        stripped = line.strip()
        if stripped.startswith("---") and stripped.endswith("---"):
            parts = stripped.strip("- ").split()
            if len(parts) == 1:
                current = parts[0]
                if current not in sections:
                    sections.append(current)
                continue
        if current and ("Error" in line or "***" in line):
            failed.add(current)
    return (sections, sorted(failed)), log


def compileWorkload(build, src, sources, kind, outdir):
    """Compile every source once with this tcc. Returns (seconds, digests)."""
    tcc = os.path.join(build, "tcc")
    base = [tcc, "-B" + build, "-I" + os.path.join(src, "include")]
    if kind == "tinycc":
        base += ["-I" + src, "-I" + build, "-DTCC_TARGET_X86_64",
                 "-DONE_SOURCE=0", '-DCONFIG_LDDIR="lib64"']
    else:
        base += ["-I" + os.path.join(ROOT, "include"),
                 "-I" + os.path.join(ROOT, "sdk", "include"),
                 "-I" + os.path.join(ROOT, ".deps", "zydis_src-src")]

    start = time.perf_counter()
    digests = {}
    for source in sources:
        obj = os.path.join(outdir, os.path.basename(source)[:-2] + ".o")
        r = subprocess.run(base + ["-w", "-c", source, "-o", obj],
                           capture_output=True, text=True)
        if r.returncode != 0:
            tail = (r.stderr.strip().splitlines() or [""])[-1]
            return None, os.path.basename(source) + ": " + tail
        digests[os.path.basename(source)] = hashlib.sha1(open(obj, "rb").read()).hexdigest()
    return time.perf_counter() - start, digests


def measure(build, src, sources, kind, runs):
    fastest, digests = None, None
    for _ in range(runs):
        tmp = tempfile.mkdtemp(prefix="bench-tcc-")
        try:
            elapsed, result = compileWorkload(build, src, sources, kind, tmp)
        finally:
            shutil.rmtree(tmp, ignore_errors=True)
        if elapsed is None:
            return None, result
        fastest = elapsed if fastest is None else min(fastest, elapsed)
        digests = result
    return fastest, digests


def textSize(path):
    if not shutil.which("size"):
        return None
    r = subprocess.run(["size", "-A", path], capture_output=True, text=True)
    for line in r.stdout.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0] == ".text":
            return int(parts[1])
    return None


def table(title, columns, rows, fmt):
    width = max(len(r[0]) for r in rows) + 2
    head = " " * width + "".join(c.rjust(12) for c in columns)
    print("\n" + title)
    print(head)
    print("-" * len(head))
    for name, values in rows:
        print(name.ljust(width) +
              "".join((fmt(v) if v is not None else "-").rjust(12) for v in values))


def sources(kind, src):
    if kind == "tinycc":
        return [os.path.join(src, f) for f in TCC_SOURCES]
    found = []
    for root, _, files in os.walk(os.path.join(ROOT, "src")):
        if "riscv64" in root:
            continue
        found += [os.path.join(root, f) for f in files if f.endswith(".c")]
    return sorted(found)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-c", "--compiler", default=os.path.join(ROOT, "build", "bin", "main"))
    ap.add_argument("-w", "--work-dir", default=os.path.join(ROOT, "build-tcc"),
                    help="clone and build directory (default: <repo>/build-tcc)")
    ap.add_argument("--reference", default="cc")
    ap.add_argument("--only", default="", help="comma-separated configurations")
    ap.add_argument("--runs", type=int, default=3)
    ap.add_argument("--no-tests", action="store_true", help="skip tinycc's test suite")
    ap.add_argument("--keep", action="store_true",
                    help="reuse an existing checkout and build tree")
    args = ap.parse_args()

    if not os.path.exists(args.compiler):
        sys.exit("no such compiler: " + args.compiler)

    wanted = [c for c in CONFIGS if not args.only or c in args.only.split(",")]
    work = os.path.abspath(args.work_dir)
    src = obtainSource(work, args.keep)
    ccdir = writeWrappers(work, os.path.abspath(args.compiler),
                          os.path.join(work, "gcc-O2"), src)

    builds, problems = {}, []
    for name in wanted:
        print("=== " + name, flush=True)
        d = os.path.join(work, name)
        if args.keep and os.path.exists(os.path.join(d, "tcc")):
            note("reusing " + d)
            builds[name] = d
            continue
        d, error = buildOne(name, work, ccdir, src, args.reference)
        if error:
            note("BUILD " + error)
            problems.append("%s: %s" % (name, error))
            continue
        note("built, tcc is %d bytes" % os.path.getsize(os.path.join(d, "tcc")))
        builds[name] = d

    ok = [c for c in wanted if c in builds]

    if not args.no_tests:
        print("\n=== tinycc's own test suite", flush=True)
        for name in ok:
            result, log = runTests(name, builds[name], ccdir, src, args.reference)
            if result is None:
                note("%-9s %s" % (name, log))
                continue
            sections, failed = result
            note("%-9s %d/%d sections passed%s"
                 % (name, len(sections) - len(failed), len(sections),
                    "   FAILED: " + " ".join(failed) if failed else ""))
            if failed:
                problems.append("%s: %s (%s)" % (name, " ".join(failed), log))

    for kind in ("tinycc", "educc"):
        files = sources(kind, src)
        print("\n=== workload: %s compiling %s (%d files)"
              % ("tcc", kind, len(files)), flush=True)
        times, digests = {}, {}
        for name in ok:
            elapsed, result = measure(builds[name], src, files, kind, args.runs)
            times[name] = elapsed
            if elapsed is None:
                note("%-9s FAILED: %s" % (name, result))
                problems.append("%s: %s workload: %s" % (name, kind, result))
            else:
                digests[name] = result

        table("compile time (s), best of %d" % args.runs, ok,
              [(kind, [times.get(c) for c in ok])], lambda v: "%.3f" % v)

        # tcc is deterministic: two tccs that disagree about an object file
        # disagree because one of them was miscompiled, and this names the file.
        agreeing = [c for c in ok if c in digests]
        if len(agreeing) > 1:
            # The reference tcc is the base when there is one: it is the column
            # nothing in this repo built, so a disagreement with it names the
            # configuration at fault rather than merely a pair that differ.
            base = next((c for c in agreeing if c.startswith("gcc-")), agreeing[0])
            disagreed = False
            for name in agreeing:
                if name == base:
                    continue
                differ = sorted(f for f in digests[base]
                                if digests[base][f] != digests[name].get(f))
                if differ:
                    disagreed = True
                    note("%-9s emits different objects than %s: %s"
                         % (name, base, " ".join(differ)))
                    problems.append("%s: objects differ from %s over %s: %s"
                                    % (name, base, kind, " ".join(differ)))
            if not disagreed:
                note("objects identical across all %d tccs" % len(agreeing))

    table(".text of the tcc binary", ok,
          [("tcc", [textSize(os.path.join(builds[c], "tcc")) for c in ok])],
          lambda v: "%.1fK" % (v / 1024.0))

    if problems:
        print("\nPROBLEMS")
        for p in problems:
            print("  " + p)
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
