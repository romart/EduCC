from os import path
import sys
import os.path
from pathlib import Path
import argparse
import subprocess
from subprocess import Popen

# No color codes when not attached to a terminal (e.g. under `ctest`, whose
# --output-junit report embeds raw stdout - ANSI escapes there show up as
# NON-XML-CHAR noise instead of rendering as color).
if sys.stdout.isatty():
    CRED    = '\33[31m'
    CGREEN  = '\33[32m'
    CYELLOW = '\33[33m'
    RESET = "\033[0;0m"
    CBOLD     = '\33[1m'
else:
    CRED = CGREEN = CYELLOW = RESET = CBOLD = ''

numOfFailedTests = 0
failedTests = []
updateBaselines = False
irPhase = 'ssa'

# Extra flags put in front of every compiler invocation ('--compiler-flag').
# This is how one set of fixtures is run against a second configuration of the
# compiler rather than being copied: today '-experimental', which routes
# codegen through the IR pipeline instead of the legacy AST walker.
compilerFlags = []

# A test is muted by putting a '<name>.muted' file next to its '<name>.c'; the
# file's contents are the reason, printed whenever the test runs. Muted tests
# still run - they are known-broken fixtures kept in the repo so a bug stays
# reproducible, and skipping them outright would mean nobody ever notices when
# one starts passing again.
#
# '<name>.muted.legacy' and '<name>.muted.experimental' mute in one
# configuration only, for a bug that is one backend's and not the other's: the
# fixture is then an ordinary passing test in the configuration that gets it
# right, and would otherwise be reported as a muted test that now passes there
# every single run.
MUTE_MARKER_EXT = '.muted'

# Reason for the test currently running, or None. Module-global for the same
# reason irPhase/updateBaselines are: every runXTest() reports its own result,
# and threading a flag through all four of them buys nothing.
currentMuteReason = None

mutedFailures = set()   # muted tests that failed, i.e. the marker is doing its job
mutedPasses = set()     # muted tests that passed - candidates for unmuting

# A '<name>.experimental' or '<name>.legacy' sibling means the fixture only
# makes sense under that one backend, and the other is not going to be taught to
# agree: a VLA in a loop, which the legacy backend's design does not really let
# it reclaim, or a fixture reading one local through a pointer to the next,
# which the IR backend is right to disagree with. Such a test is *skipped* in
# the other configuration rather than muted - muting is for a bug someone means
# to fix, so a muted test still runs and is flagged the day it starts passing,
# and neither of those is true here. The file's contents are the reason.
ONLY_MARKER_EXTS = {'experimental': '.experimental', 'legacy': '.legacy'}

skippedTests = set()


def currentConfiguration():
    """'experimental' or 'legacy', which is what the per-configuration markers
    name. Read off the compiler flags rather than passed around, because a
    marker is consulted from four different runXTest() paths."""
    return 'experimental' if '-experimental' in compilerFlags else 'legacy'


def readMarker(markerPath):
    if not path.exists(markerPath):
        return None
    with open(markerPath) as marker:
        reason = marker.read().strip()
    return reason if reason else '(no reason recorded in the marker file)'


def muteMarkerPath(dirname, name):
    """The marker that mutes this test in this configuration, whether or not it
    exists: the unqualified one when it is there, otherwise the qualified one.
    Reported to the user as the file to delete, so it has to name the one that
    is actually doing the muting."""
    unqualified = dirname + '/' + name + MUTE_MARKER_EXT
    if path.exists(unqualified):
        return unqualified
    return unqualified + '.' + currentConfiguration()


def readMuteReason(dirname, name):
    return readMarker(muteMarkerPath(dirname, name))


def readOtherConfigurationReason(dirname, name):
    """The reason this fixture is skipped here, or None to run it. A
    '<name>.experimental' or '<name>.legacy' marker names the one configuration
    the fixture belongs to; every other configuration skips it."""
    for configuration, ext in ONLY_MARKER_EXTS.items():
        if configuration == currentConfiguration():
            continue
        reason = readMarker(dirname + '/' + name + ext)
        if reason is not None:
            return configuration, reason
    return None


def failTest(testFilePath, headline):
    """Reports a failed check. Muted tests report but do not count."""
    if currentMuteReason is not None:
        print(CBOLD + CYELLOW + f"Test {testFilePath} -- FAIL (muted)" + RESET)
        print(f"  {headline}")
        mutedFailures.add(testFilePath)
        return

    print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
    print(f"  {headline}")
    recordFailure(testFilePath)


def passTest(testFilePath):
    """Reports a passing check, flagging one that was expected to fail."""
    if currentMuteReason is not None:
        print(CBOLD + CYELLOW + f"Test {testFilePath} -- OK (muted)" + RESET)
        mutedPasses.add(testFilePath)
        return

    print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)


def recordFailure(testFilePath):
    global numOfFailedTests
    numOfFailedTests = numOfFailedTests + 1
    failedTests.append(testFilePath)


def compareFilesLineByLine(marker, testFile, actualFile, expectedFile):
    with open(expectedFile) as expected, open(actualFile) as actual:
        expt = expected.readlines()
        actl = actual.readlines()

        if len(expt) != len(actl):
            failTest(testFile, f"{marker}: actual output len ({len(actl)}) differs from expected len ({len(expt)})")
            return False

        for i in range(len(expt)):
            e = expt[i].rstrip()
            a = actl[i].rstrip()
            if (e != a):
                failTest(testFile, f"{marker}: actual output differs from expected in line {i + 1}")
                print(f"  ACTUAL:   {a}")
                print(f"  EXPECTED: {e}")
                return False
    return True


def checkOrUpdateBaseline(marker, testFile, actualFile, expectedFile):
    # With --update-baselines, always accept the actual output as the new
    # baseline instead of comparing (this is the only way baselines are
    # written - a missing baseline no longer auto-passes and auto-creates
    # itself, since that made it too easy to silently bake in a regression).
    if updateBaselines:
        # Never bake a muted test's output into a baseline: muted means the
        # output is known to be wrong, so recording it would turn the bug into
        # the expected result and make the test fail once it is fixed.
        if currentMuteReason is not None:
            print(f"  info: {marker} baseline left alone ({expectedFile}); test is muted")
            return True

        existed = path.exists(expectedFile)
        content = open(actualFile).read()
        open(expectedFile, 'w+').write(content)
        print(f"  info: {marker} baseline {'updated' if existed else 'created'} ({expectedFile})")
        return True

    if not path.exists(expectedFile):
        failTest(testFile, f"{marker}: no baseline file ({expectedFile}); run with --update-baselines to create one")
        return False

    return compareFilesLineByLine(marker, testFile, actualFile, expectedFile)


def runParserTest(compiler, workingDir, dirname, name):
    testFilePath = dirname + '/' + name + '.c'
    expectedAstFilePath = dirname + '/' + name + '.txt'
    expectedErrFilePath = dirname + '/' + name + '.err'
    expectedAstCanonFilePath = dirname + '/' + name + '.canon.txt'
    outputDir = workingDir + '/' + dirname

    if (not path.exists(outputDir)):
        os.makedirs(outputDir)

    actualAstFilePath = workingDir + '/' + expectedAstFilePath
    actualErrFilePath = workingDir + '/' + expectedErrFilePath
    actualAstCanonFilePath = workingDir + '/' + expectedAstCanonFilePath

    err = open(actualErrFilePath, 'w+')

    compilationCommand = [compiler, "-skipCodegen", "-oneline", "-astDump", actualAstFilePath, "-astCanonDump", actualAstCanonFilePath, testFilePath]
    process = Popen(compilationCommand, stdout=subprocess.DEVNULL, stderr=err)
    exit_code = process.wait()
    err.close()

    # A positive exit code just means the compiler reported diagnostics
    # (expected for tests under parser/negative) - only a negative code
    # (killed by a signal, e.g. a crash/assertion) is an actual tooling
    # failure here.
    if exit_code < 0:
        failTest(testFilePath, f"Process crashed (signal {-exit_code})")
        return

    testOk = checkOrUpdateBaseline("AstDump", testFilePath, actualAstFilePath, expectedAstFilePath)

    if testOk:
        testOk = checkOrUpdateBaseline("Stderr", testFilePath, actualErrFilePath, expectedErrFilePath)

    # Negative tests have errors, so canonicalization (and thus the canon
    # dump) never runs for them - only compare it when the compiler
    # actually produced one.
    if testOk and path.exists(actualAstCanonFilePath):
        testOk = checkOrUpdateBaseline("AstCanonDump", testFilePath, actualAstCanonFilePath, expectedAstCanonFilePath)

    if testOk:
        passTest(testFilePath)


def runCodegenTest(compiler, workingDir, dirname, name):
    testFilePath = dirname + '/' + name + '.c'
    argsFilePath = dirname + '/' + name + '.args'

    outputDir = workingDir + '/' + dirname

    if (not path.exists(outputDir)):
        os.makedirs(outputDir)

    errFilePath = outputDir + '/' + name + '.err'
    binFileName = outputDir + '/' + name

    if path.exists(binFileName):
        os.remove(binFileName)

    args = []
    if path.exists(argsFilePath):
        with open(argsFilePath) as argsFile:
            for argLine in argsFile:
                args.append(argLine.strip())
    else:
        args.append("")

    err = open(errFilePath, 'w+')
    compilationCommand = [compiler] + compilerFlags \
                       + ["-oneline", "-o", binFileName, testFilePath, "-lm"]
    compilation = Popen(compilationCommand, stdout=sys.stdout, stderr=err)
    exit_code = compilation.wait()
    err.close()

    # Codegen fixtures are expected to compile cleanly - a nonzero exit
    # (whether "had diagnostic errors" or "killed by a signal") is a real
    # failure here, unlike in the parser/pp suites which also exercise the
    # error-reporting paths on purpose.
    if exit_code != 0:
        failTest(testFilePath, f"Compilation failed (exit code {exit_code})")
        with open(errFilePath, 'r') as f:
            output = f.read()
            if output:
                print(output)
        return

    if path.getsize(errFilePath) > 0:
        # Exit code 0 means these are warnings, not errors - don't fail the
        # test over them, but surface them since they're still worth seeing.
        with open(errFilePath, 'r') as f:
            print(f"  warning: compiler produced diagnostics on a successful compile:")
            print(f.read())

    for arg in args:
        runCommand = [binFileName]
        if arg:
            runCommand.extend(arg.split())
        execution = Popen(runCommand, stdout=sys.stdout, stderr=sys.stderr)
        exit_code = execution.wait()
        if exit_code != 0:
            failTest(testFilePath, f"Execution exit code is not 0 ({exit_code})")
            if arg:
                print(f"  Argument: '{arg}'")
        else:
            passTest(testFilePath)


def runPPTest(compiler, workingDir, dirname, name):
    testFilePath = dirname + '/' + name + '.c'
    expectFilePath = dirname + '/' + name + '.expect'

    outputDir = workingDir + '/' + dirname

    if (not path.exists(outputDir)):
        os.makedirs(outputDir)

    actualFilePath = outputDir + '/' + name + '.actual'

    if path.exists(actualFilePath):
        os.remove(actualFilePath)

    out = open(actualFilePath, 'w+')

    compilationCommand = [compiler, "-E", testFilePath]
    compilation = Popen(compilationCommand, stdout=out, stderr=sys.stderr)
    exit_code = compilation.wait()
    out.close()

    if exit_code < 0:
        failTest(testFilePath, f"Process crashed (signal {-exit_code})")
        return

    testOk = checkOrUpdateBaseline("preprocessed", testFilePath, actualFilePath, expectFilePath)

    if testOk:
        passTest(testFilePath)


def runIrTest(compiler, workingDir, dirname, name):
    # Snapshots the IR right after the pass selected by --ir-phase (via
    # '-irDump:<phase>', see -irDump:phase[,phase...] in src/main.c) so each
    # fixture suite tests one pass in isolation, unaffected by whatever the
    # later passes do to the IR afterwards - e.g. the ir/ssa suite runs with
    # 'ssa' (right after buildSSA), the ir/gvn suite with 'gvn'.
    testFilePath = dirname + '/' + name + '.c'
    expectedIrFilePath = dirname + '/' + name + '.' + irPhase + '.txt'
    expectedErrFilePath = dirname + '/' + name + '.err'

    outputDir = workingDir + '/' + dirname

    if (not path.exists(outputDir)):
        os.makedirs(outputDir)

    actualIrFilePath = workingDir + '/' + expectedIrFilePath
    actualErrFilePath = workingDir + '/' + expectedErrFilePath

    err = open(actualErrFilePath, 'w+')

    compilationCommand = [compiler, "-experimental", "-skipCodegen", "-oneline", "-irDump:" + irPhase, actualIrFilePath, testFilePath]
    process = Popen(compilationCommand, stdout=subprocess.DEVNULL, stderr=err)
    exit_code = process.wait()
    err.close()

    # These fixtures are expected to translate to IR cleanly, same
    # contract as the codegen suite - a nonzero exit is a real failure.
    if exit_code != 0:
        failTest(testFilePath, f"Translation failed (exit code {exit_code})")
        with open(actualErrFilePath, 'r') as f:
            output = f.read()
            if output:
                print(output)
        return

    if path.getsize(actualErrFilePath) > 0:
        with open(actualErrFilePath, 'r') as f:
            print(f"  warning: compiler produced diagnostics on a successful translation:")
            print(f.read())

    testOk = checkOrUpdateBaseline("IrDump:" + irPhase, testFilePath, actualIrFilePath, expectedIrFilePath)

    if testOk:
        passTest(testFilePath)


def runTestForData(filePath, compiler, workingDir, testMode):
    global currentMuteReason

    basename = os.path.basename(filePath)
    dirname = os.path.dirname(filePath)
    name, ext = os.path.splitext(basename)
    if (ext != ".c"):
        return

    belongsTo = readOtherConfigurationReason(dirname, name)
    if belongsTo is not None:
        configuration, reason = belongsTo
        print(CBOLD + CYELLOW +
              f"Test {dirname}/{name}.c -- SKIP (needs {configuration}): {reason.splitlines()[0]}" +
              RESET)
        skippedTests.add(filePath)
        return

    currentMuteReason = readMuteReason(dirname, name)
    if currentMuteReason is not None:
        reasonLines = currentMuteReason.splitlines()
        print(CBOLD + CYELLOW + f"Test {dirname}/{name}.c -- muted: {reasonLines[0]}" + RESET)
        for reasonLine in reasonLines[1:]:
            print(f"    {reasonLine}")

    try:
        if (testMode == 'parser'):
            runParserTest(compiler, workingDir, dirname, name)
        elif testMode == 'preprocessor':
            runPPTest(compiler, workingDir, dirname, name)
        elif testMode == 'codegen':
            runCodegenTest(compiler, workingDir, dirname, name)
        elif testMode == 'ir':
            runIrTest(compiler, workingDir, dirname, name)
        else:
            raise Exception(f"Unknown test mode {testMode}")
    finally:
        currentMuteReason = None


def walkDirectory(dirPath, indent, block):
    # Sorted so run order (and thus failure order in the output) is
    # deterministic across machines/runs instead of depending on whatever
    # order the filesystem happens to hand back.
    for file in sorted(dirPath.iterdir()):
        if file.is_dir():
            walkDirectory(file, indent + 1, block)
        else:
            block(file)


def parseArguments():
    parser = argparse.ArgumentParser(
        description="Runs all or a subset of the ART test suite. A test is muted by placing a "
                    "'<name>.muted' file next to its '<name>.c', with the reason as its contents; "
                    "a muted test still runs and reports, but its failures do not count towards "
                    "the exit code, and it is called out in the summary if it starts passing. "
                    "'<name>.muted.experimental' and '<name>.muted.legacy' mute in one "
                    "configuration only, for a bug that belongs to one backend. A "
                    "'<name>.experimental' or '<name>.legacy' sibling means the fixture belongs to "
                    "that backend alone, and skips it in every other configuration.")
    parser.add_argument('-c', '--compiler', type=str, required=True, help="specify path to compiler")
    parser.add_argument('-wd', '--working-dir', type=str, required=True, help="specify working dir for tests")
    parser.add_argument('-p', '--test-path', type=str, required=True, action='append', help='path to test')
    parser.add_argument('-m', '--mode', choices=['parser', 'preprocessor', 'codegen', 'ir'], default='parser', help='Which substystem to be tested')
    parser.add_argument('--ir-phase', choices=['initial', 'ssa', 'scp', 'gvn', 'dce', 'mir', 'isel', 'ra'], default='ssa',
                         help="which pipeline phase 'ir' mode snapshots (selects the -irDump:<phase> flag "
                              "and the <name>.<phase>.txt baseline suffix). 'mir' and 'isel' are the odd "
                              "ones out: they dump the MachineFunction rather than the IR - 'mir' as stage 0 "
                              "leaves it, 'isel' once instruction selection has filled the blocks in")
    parser.add_argument('--compiler-flag', type=str, action='append', default=[],
                         help='extra flag passed to the compiler on every invocation, repeatable '
                              '(e.g. --compiler-flag -experimental to compile the fixtures through '
                              'the IR pipeline)')
    parser.add_argument('--update-baselines', action='store_true',
                         help='write actual output as the new expected baseline for every test instead of comparing '
                              '(use after an intentional behavior change to regenerate golden files)')

    return parser.parse_args()


def main():
    global updateBaselines
    global irPhase
    global compilerFlags

    args = parseArguments()
    compilerFlags = args.compiler_flag
    testMode = args.mode
    testPaths = args.test_path
    workingDir = args.working_dir
    compiler = args.compiler
    updateBaselines = args.update_baselines
    irPhase = args.ir_phase

    for testPath in testPaths:
        p = Path(testPath)
        if p.exists():
            if p.is_dir():
                walkDirectory(p, 0, lambda a: runTestForData(a, compiler, workingDir, testMode))

    if numOfFailedTests:
        print(CBOLD + CRED + f"Failed tests: {numOfFailedTests}" + RESET)
        for t in failedTests:
            print(f"  {t}")
    else:
        print(CBOLD + CGREEN + f"All tests passed" + RESET)

    if skippedTests:
        print(CBOLD + CYELLOW +
              f"Skipped (belong to the other backend): {len(skippedTests)}" + RESET)
        for t in sorted(skippedTests):
            print(f"  {t}")

    if mutedFailures:
        print(CBOLD + CYELLOW + f"Muted tests still failing (not counted): {len(mutedFailures)}" + RESET)
        for t in sorted(mutedFailures):
            print(f"  {t}")

    # A muted test that passes every check means the bug it pins down is fixed
    # and the marker is now hiding a working test. Reported loudly but not
    # counted as a failure: turning someone's bug fix into a red build would
    # only teach them to delete the fixture. A test that failed at least one
    # check is still doing its job, even if another check passed.
    #
    # Not reported under --update-baselines: that run compares nothing, so
    # every baseline-driven test "passes" and a muted one would be flagged
    # stale on no evidence at all.
    stalelyMuted = set() if updateBaselines else mutedPasses - mutedFailures
    if stalelyMuted:
        print(CBOLD + CYELLOW + f"MUTED TESTS THAT NOW PASS: {len(stalelyMuted)}" + RESET)
        for t in sorted(stalelyMuted):
            stale = muteMarkerPath(os.path.dirname(t),
                                   os.path.splitext(os.path.basename(t))[0])
            print(f"  {t} -- delete {stale} to unmute it")

    exit(numOfFailedTests)


if __name__ == "__main__":
    main()
