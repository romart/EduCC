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
    RESET = "\033[0;0m"
    CBOLD     = '\33[1m'
else:
    CRED = CGREEN = RESET = CBOLD = ''

numOfFailedTests = 0
failedTests = []
updateBaselines = False


def recordFailure(testFilePath):
    global numOfFailedTests
    numOfFailedTests = numOfFailedTests + 1
    failedTests.append(testFilePath)


def compareFilesLineByLine(marker, testFile, actualFile, expectedFile):
    with open(expectedFile) as expected, open(actualFile) as actual:
        expt = expected.readlines()
        actl = actual.readlines()

        if len(expt) != len(actl):
            print(CBOLD + CRED + f"Test {testFile} -- FAIL" + RESET)
            print(f" {marker}: actual output len ({len(actl)}) differs from expected len ({len(expt)})")
            recordFailure(testFile)
            return False

        for i in range(len(expt)):
            e = expt[i].rstrip()
            a = actl[i].rstrip()
            if (e != a):
                print(CBOLD + CRED + f"Test {testFile} -- FAIL" + RESET)
                print(f" {marker}: actual output differs from expected in line {i + 1}")
                print(f"  ACTUAL:   {a}")
                print(f"  EXPECTED: {e}")
                recordFailure(testFile)
                return False
    return True


def checkOrUpdateBaseline(marker, testFile, actualFile, expectedFile):
    # With --update-baselines, always accept the actual output as the new
    # baseline instead of comparing (this is the only way baselines are
    # written - a missing baseline no longer auto-passes and auto-creates
    # itself, since that made it too easy to silently bake in a regression).
    if updateBaselines:
        existed = path.exists(expectedFile)
        content = open(actualFile).read()
        open(expectedFile, 'w+').write(content)
        print(f"  info: {marker} baseline {'updated' if existed else 'created'} ({expectedFile})")
        return True

    if not path.exists(expectedFile):
        print(CBOLD + CRED + f"Test {testFile} -- FAIL" + RESET)
        print(f"  {marker}: no baseline file ({expectedFile}); run with --update-baselines to create one")
        recordFailure(testFile)
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
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"  Process crashed (signal {-exit_code})")
        recordFailure(testFilePath)
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
        print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)


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
    compilationCommand = [compiler, "-oneline", "-o", binFileName, testFilePath, "-lm"]
    compilation = Popen(compilationCommand, stdout=sys.stdout, stderr=err)
    exit_code = compilation.wait()
    err.close()

    # Codegen fixtures are expected to compile cleanly - a nonzero exit
    # (whether "had diagnostic errors" or "killed by a signal") is a real
    # failure here, unlike in the parser/pp suites which also exercise the
    # error-reporting paths on purpose.
    if exit_code != 0:
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"  Compilation failed (exit code {exit_code})")
        with open(errFilePath, 'r') as f:
            output = f.read()
            if output:
                print(output)
        recordFailure(testFilePath)
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
            print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
            print(f"  Execution exit code is not 0 ({exit_code})")
            if arg:
                print(f"  Argument: '{arg}'")
            recordFailure(testFilePath)
        else:
            print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)


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
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"  Process crashed (signal {-exit_code})")
        recordFailure(testFilePath)
        return

    testOk = checkOrUpdateBaseline("preprocessed", testFilePath, actualFilePath, expectFilePath)

    if testOk:
        print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)


def runTestForData(filePath, compiler, workingDir, testMode):
    basename = os.path.basename(filePath)
    dirname = os.path.dirname(filePath)
    name, ext = os.path.splitext(basename)
    if (ext == ".c"):
        if (testMode == 'parser'):
            runParserTest(compiler, workingDir, dirname, name)
        elif testMode == 'preprocessor':
            runPPTest(compiler, workingDir, dirname, name)
        elif testMode == 'codegen':
            runCodegenTest(compiler, workingDir, dirname, name)
        else:
            raise Exception(f"Unknown test mode {testMode}")


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
    parser = argparse.ArgumentParser(description="Runs all or a subset of the ART test suite.")
    parser.add_argument('-c', '--compiler', type=str, required=True, help="specify path to compiler")
    parser.add_argument('-wd', '--working-dir', type=str, required=True, help="specify working dir for tests")
    parser.add_argument('-p', '--test-path', type=str, required=True, action='append', help='path to test')
    parser.add_argument('-m', '--mode', choices=['parser', 'preprocessor', 'codegen'], default='parser', help='Which substystem to be tested')
    parser.add_argument('--update-baselines', action='store_true',
                         help='write actual output as the new expected baseline for every test instead of comparing '
                              '(use after an intentional behavior change to regenerate golden files)')

    return parser.parse_args()


def main():
    global updateBaselines

    args = parseArguments()
    testMode = args.mode
    testPaths = args.test_path
    workingDir = args.working_dir
    compiler = args.compiler
    updateBaselines = args.update_baselines

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

    exit(numOfFailedTests)


if __name__ == "__main__":
    main()
