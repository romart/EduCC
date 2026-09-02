#!/usr/bin/env python3
"""Compare EduCC's code generators against each other, and against a real one.

    python3 test/bench/bench.py                       # everything, defaults
    python3 test/bench/bench.py --runs 5 --json a.json
    python3 test/bench/bench.py --compare a.json      # against a saved run
    python3 test/bench/bench.py --filter nbody,sort   # some programs only

Three questions, which are not the same question:

  * how fast is the code EduCC produces      - programs/*.c, compiled by each
                                               configuration and run
  * how fast is EduCC                        - the compiler's own sources,
                                               compiled by each configuration
  * how big is what it produces              - .text of the linked binary

The configurations are the two backends and, within the IR one, each register
allocator, plus the host compiler at -O0 and -O2 as a reference. The reference
is not a target: it is there so that a number like "2.4 seconds" means
something, and it doubles as the oracle for whether a benchmark still computes
the right answer - every configuration's output has to agree with it.

Nothing here is wired into ctest. It takes a couple of minutes, it measures
wall-clock time, and a shared machine will happily lie to it; it is a thing to
run deliberately, before and after a change to the backend, with --compare.
"""
import argparse
import json
import os
import platform
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
PROGRAMS = os.path.join(HERE, "programs")

# The compiler's own sources, as the input to the "how fast is EduCC" half.
# Deliberately the big front-end and IR files and nothing that includes
# <Zydis.h>: the point is throughput over real C, not a self-host.
SELF_SOURCES = [
    "src/parser.c", "src/sema.c", "src/pp.c",
    "src/ir/ast2ir.c", "src/ir/gvn.c", "src/ir/codegen/isel.c",
]


class Config:
    def __init__(self, name, compiler, flags, isEduCC):
        self.name = name
        self.compiler = compiler
        self.flags = flags
        self.isEduCC = isEduCC

    def compileCommand(self, source, out, link):
        cmd = [self.compiler] + self.flags
        if self.isEduCC:
            cmd.append("-oneline")
        if link:
            return cmd + ["-o", out, source, "-lm"]
        return cmd + ["-c", "-o", out, source]


def configurations(compiler, reference):
    cfgs = [
        Config("legacy", compiler, ["-legacy"], True),
        Config("trivial", compiler, ["-Xregalloc=trivial"], True),
        Config("linear", compiler, ["-Xregalloc=linear"], True),
        Config("chaitin", compiler, ["-Xregalloc=chaitin"], True),
    ]
    if reference:
        cfgs.append(Config(reference + " -O0", reference, ["-O0"], False))
        cfgs.append(Config(reference + " -O2", reference, ["-O2"], False))
    return cfgs


def best(runs, thunk):
    """Fastest of `runs` attempts, and the last result. Fastest, not mean: the
    thing being measured has a floor and no ceiling, so a slow sample is the
    machine talking, not the compiler."""
    lowest, result = None, None
    for _ in range(runs):
        start = time.perf_counter()
        result = thunk()
        elapsed = time.perf_counter() - start
        if lowest is None or elapsed < lowest:
            lowest = elapsed
    return lowest, result


def textSize(path):
    """Bytes of .text in an object or executable, or None if `size` is missing."""
    if not shutil.which("size"):
        return None
    r = subprocess.run(["size", "-A", path], capture_output=True, text=True)
    if r.returncode != 0:
        return None
    for line in r.stdout.splitlines():
        parts = line.split()
        if len(parts) >= 2 and parts[0] == ".text":
            return int(parts[1])
    return None


def programs(patterns):
    names = sorted(f[:-2] for f in os.listdir(PROGRAMS) if f.endswith(".c"))
    if patterns:
        names = [n for n in names if any(p in n for p in patterns)]
    return names


def measureProgram(cfg, name, runs, tmp):
    """Compile, link and run one benchmark under one configuration."""
    source = os.path.join(PROGRAMS, name + ".c")
    binary = os.path.join(tmp, name + "." + cfg.name.replace(" ", "_"))
    entry = {"compile": None, "run": None, "text": None, "output": None,
             "error": None}

    def doCompile():
        return subprocess.run(cfg.compileCommand(source, binary, True),
                              capture_output=True)

    entry["compile"], r = best(runs, doCompile)
    if r.returncode != 0 or not os.path.exists(binary):
        entry["error"] = "compile failed: " + r.stderr.decode(errors="replace").strip()[:200]
        return entry

    entry["text"] = textSize(binary)

    def doRun():
        return subprocess.run([binary], capture_output=True)

    entry["run"], r = best(runs, doRun)
    if r.returncode != 0:
        entry["error"] = f"exited {r.returncode}"
        return entry
    entry["output"] = r.stdout.decode(errors="replace").strip()
    return entry


def measureSelfCompile(cfg, runs, tmp):
    """Compile EduCC's own sources to objects. No link: this is throughput."""
    entry = {"compile": None, "text": None, "error": None}
    objs = [os.path.join(tmp, "self_" + cfg.name.replace(" ", "_")
                         + "_" + s.replace("/", "_") + ".o") for s in SELF_SOURCES]

    def doCompile():
        for source, obj in zip(SELF_SOURCES, objs):
            cmd = cfg.compileCommand(os.path.join(ROOT, source), obj, False)
            cmd += ["-I", os.path.join(ROOT, "include"),
                    "-I", os.path.join(ROOT, "sdk", "include")]
            r = subprocess.run(cmd, capture_output=True, cwd=ROOT)
            if r.returncode != 0:
                return r
        return None

    entry["compile"], failure = best(runs, doCompile)
    if failure is not None:
        entry["compile"] = None
        entry["error"] = "compile failed: " + failure.stderr.decode(errors="replace").strip()[:200]
        return entry

    sizes = [textSize(o) for o in objs]
    entry["text"] = sum(s for s in sizes if s is not None) if all(sizes) else None
    return entry


def measureStatic(cfg, names, tmp):
    """Spill slots and machine instructions the allocator leaves behind.

    A wall clock says one configuration is slower; this says what it did. Only
    the IR backend has an -irDump to ask, so -legacy has no row here.
    """
    if not cfg.isEduCC or "-legacy" in cfg.flags:
        return None

    spills, instrs = 0, 0
    dump = os.path.join(tmp, "ra_" + cfg.name.replace(" ", "_") + ".txt")
    sources = [os.path.join(PROGRAMS, n + ".c") for n in names]
    sources += [os.path.join(ROOT, s) for s in SELF_SOURCES]

    for source in sources:
        cmd = [cfg.compiler] + cfg.flags + [
            "-skipCodegen", "-oneline", "-irDump:ra", dump,
            "-I", os.path.join(ROOT, "include"),
            "-I", os.path.join(ROOT, "sdk", "include"), source]
        if subprocess.run(cmd, capture_output=True, cwd=ROOT).returncode != 0:
            continue
        if not os.path.exists(dump):
            continue
        with open(dump) as f:
            inBlock = False
            for line in f:
                if line.startswith("MBB #"):
                    inBlock = True
                elif not line.strip():
                    inBlock = False
                elif inBlock:
                    instrs += 1
                elif ": spill " in line:
                    spills += 1
        os.remove(dump)

    return {"spillSlots": spills, "instructions": instrs}


def progress(text):
    # Only when someone is watching: piped into a file or a CI log, a carriage
    # return is not a carriage return and every step ends up on one long line.
    if sys.stderr.isatty():
        print("  " + text.ljust(56), end="\r", file=sys.stderr, flush=True)


def table(title, columns, rows, fmt, total=None):
    width = max([len(r[0]) for r in rows] + [len(title), 12])
    head = title.ljust(width) + "".join(c.rjust(12) for c in columns)
    print()
    print(head)
    print("-" * len(head))
    for name, values in rows:
        cells = "".join(fmt(v).rjust(12) for v in values)
        print(name.ljust(width) + cells)
    if total is not None:
        print("-" * len(head))
        print("total".ljust(width) + "".join(fmt(v).rjust(12) for v in total))


def seconds(v):
    return "-" if v is None else f"{v:.3f}"


def kilobytes(v):
    return "-" if v is None else f"{v / 1024.0:.1f}K"


def count(v):
    return "-" if v is None else str(v)


def sumOrNone(values):
    present = [v for v in values if v is not None]
    return sum(present) if len(present) == len(values) else None


def report(results, configs, names):
    cols = [c.name for c in configs]

    for metric, fmt in (("run", seconds), ("compile", seconds), ("text", kilobytes)):
        rows = []
        for n in names:
            rows.append((n, [results["programs"][n][c.name][metric] for c in configs]))
        totals = [sumOrNone([results["programs"][n][c.name][metric] for n in names])
                  for c in configs]
        titles = {"run": "run time (s)", "compile": "compile time (s)",
                  "text": ".text size"}
        table(titles[metric], cols, rows, fmt, totals)

    self_ = results["selfCompile"]
    if any(self_[c.name]["compile"] is not None for c in configs):
        table("EduCC's own sources", cols,
              [("compile time (s)", [self_[c.name]["compile"] for c in configs])],
              seconds)
        table("", cols,
              [(".text size", [self_[c.name]["text"] for c in configs])],
              kilobytes)

    static = results["static"]
    if any(static.get(c.name) for c in configs):
        rows = [("spill slots", [static.get(c.name, {}).get("spillSlots") if static.get(c.name) else None
                                 for c in configs]),
                ("instructions", [static.get(c.name, {}).get("instructions") if static.get(c.name) else None
                                  for c in configs])]
        table("what the allocator did", cols, rows, count)

    problems = []
    for n in names:
        for c in configs:
            e = results["programs"][n][c.name]
            if e["error"]:
                problems.append(f"{n} / {c.name}: {e['error']}")
        outputs = {c.name: results["programs"][n][c.name]["output"] for c in configs}
        agreed = set(v for v in outputs.values() if v is not None)
        if len(agreed) > 1:
            for k, v in outputs.items():
                problems.append(f"{n} / {k}: printed {v!r}")
    for c in configs:
        if self_[c.name]["error"]:
            problems.append(f"EduCC's own sources / {c.name}: {self_[c.name]['error']}")

    if problems:
        print()
        print("PROBLEMS")
        for p in problems:
            print("  " + p)
    return 1 if problems else 0


def compare(results, previous, configs, names):
    """Percent change against a saved run, for the metrics worth watching."""
    print()
    print("=== against " + previous.get("label", "the saved run"))

    def delta(new, old):
        if new is None or old is None or old == 0:
            return None
        return (new - old) / old * 100.0

    def percent(v):
        return "-" if v is None else f"{v:+.1f}%"

    for metric, section in (("run", "programs"), ("compile", "programs"),
                            ("text", "programs")):
        rows = []
        for n in names:
            if n not in previous.get(section, {}):
                continue
            values = []
            for c in configs:
                new = results[section][n].get(c.name, {}).get(metric)
                old = previous[section][n].get(c.name, {}).get(metric)
                values.append(delta(new, old))
            rows.append((n, values))
        if rows:
            table(metric + " delta", [c.name for c in configs], rows, percent)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-c", "--compiler", default=os.path.join(ROOT, "build", "bin", "main"),
                    help="the EduCC binary to measure (default build/bin/main)")
    ap.add_argument("--reference", default="cc",
                    help="host compiler to measure alongside, or '' for none (default cc)")
    ap.add_argument("--runs", type=int, default=3,
                    help="attempts per measurement; the fastest counts (default 3)")
    ap.add_argument("--filter", default="",
                    help="comma-separated substrings; only matching programs run")
    ap.add_argument("--json", default=None, help="write the full results here")
    ap.add_argument("--compare", default=None, help="a previous --json to diff against")
    ap.add_argument("--label", default=None, help="name this run in the JSON")
    ap.add_argument("--no-self-compile", action="store_true",
                    help="skip the compiler-throughput half")
    ap.add_argument("--no-static", action="store_true",
                    help="skip the spill-slot and instruction counts")
    args = ap.parse_args()

    if not os.path.exists(args.compiler):
        print(f"no such compiler: {args.compiler}", file=sys.stderr)
        return 2
    reference = args.reference if args.reference and shutil.which(args.reference) else None
    configs = configurations(os.path.abspath(args.compiler), reference)
    names = programs([p for p in args.filter.split(",") if p])
    if not names:
        print("no benchmark programs selected", file=sys.stderr)
        return 2

    results = {
        "label": args.label or time.strftime("%Y-%m-%d %H:%M:%S"),
        "host": platform.platform(),
        "compiler": os.path.abspath(args.compiler),
        "runs": args.runs,
        "programs": {},
        "selfCompile": {},
        "static": {},
    }

    with tempfile.TemporaryDirectory() as tmp:
        for n in names:
            results["programs"][n] = {}
            for c in configs:
                progress(f"{n} / {c.name}")
                results["programs"][n][c.name] = measureProgram(c, n, args.runs, tmp)

        for c in configs:
            progress(f"EduCC's own sources / {c.name}")
            results["selfCompile"][c.name] = (
                {"compile": None, "text": None, "error": None} if args.no_self_compile
                else measureSelfCompile(c, args.runs, tmp))

        for c in configs:
            progress(f"static / {c.name}")
            results["static"][c.name] = (None if args.no_static
                                         else measureStatic(c, names, tmp))
    progress("")

    rc = report(results, configs, names)

    if args.json:
        with open(args.json, "w") as f:
            json.dump(results, f, indent=2, sort_keys=True)
        print()
        print("written: " + args.json)

    if args.compare:
        with open(args.compare) as f:
            compare(results, json.load(f), configs, names)

    return rc


if __name__ == "__main__":
    sys.exit(main())
