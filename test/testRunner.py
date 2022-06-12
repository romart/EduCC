
from asyncio.subprocess import DEVNULL
from os import path
from re import S
import sys
import os.path
from pathlib import Path
import argparse
import subprocess
from subprocess import Popen, PIPE

CRED    = '\33[31m'
CGREEN  = '\33[32m'
RESET = "\033[0;0m"
CBOLD     = '\33[1m'

numOfFailedTests=0


def compareFilesLineByLine(marker, testFile, actualFile, expectedFile):
    global numOfFailedTests
    with open(expectedFile) as expected, open(actualFile) as actual:
        expt = expected.readlines()
        actl = actual.readlines()

        if len(expt) != len(actl):
            print(CBOLD + CRED + f"Test {testFile} -- FAIL" + RESET)
            print(f" {marker}: actual output len ({len(actl)}) differs from expected len ({len(expt)})")
            numOfFailedTests = numOfFailedTests + 1
            return False
        else:
            i = 0
            
            while (i < len(expt)):
                e = expt[i].rstrip()
                a = actl[i].rstrip()
                if (e != a):
                    print(CBOLD + CRED + f"Test {testFile} -- FAIL" + RESET)
                    print(f" {marker}: actual output differs from expected in line {i + 1}")
                    print(f"  ACTUAL:   {a}")
                    print(f"  EXPECTED: {e}")
                    numOfFailedTests = numOfFailedTests + 1
                    return False
                i = i + 1
    return True

def updateExpectedFromActualIfNeed(marker, actualFile, expectedFile):
    if (not path.exists(expectedFile)):
        print(f"  info: no {marker} expected file, create it")
        result = open(actualFile).read()
        open(expectedFile, 'w+').write(result)


def runParserTest(compiler, workingDir, dirname, name):
    global numOfFailedTests
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

    # print(f"Run process: {compiler} -astDump {actualFilePath} {testFilePath}")
    process = Popen([compiler, "-skipCodegen", "-oneline" , "-astDump", actualAstFilePath, "-astCanonDump", actualAstCanonFilePath, testFilePath], stdout=DEVNULL, stderr=err)
    exit_code = process.wait()
    if exit_code != 0:
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"  Process crashed (exit code {exit_code})")
        numOfFailedTests = numOfFailedTests + 1
    else:
        testOk = True
        if (path.exists(expectedAstFilePath)):
            testOk = compareFilesLineByLine("AstDump", testFilePath, actualAstFilePath, expectedAstFilePath)

        if (testOk and path.exists(expectedErrFilePath)):
            testOk = compareFilesLineByLine("Stderr", testFilePath, actualErrFilePath, expectedErrFilePath)

        if (testOk and path.exists(actualAstCanonFilePath) and path.exists(expectedAstCanonFilePath)):
            testOk = compareFilesLineByLine("AstCanonDump", testFilePath, actualAstCanonFilePath, expectedAstCanonFilePath)

        if (testOk):
            print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)

        updateExpectedFromActualIfNeed("AstDump", actualAstFilePath, expectedAstFilePath)
        updateExpectedFromActualIfNeed("Stderr", actualErrFilePath, expectedErrFilePath)
        if (path.exists(actualAstCanonFilePath)):
            updateExpectedFromActualIfNeed("AstCanonDump", actualAstCanonFilePath, expectedAstCanonFilePath)


def runCodegenTest(compiler, workingDir, dirname, name):
    global numOfFailedTests
    testFilePath = dirname + '/' + name + '.c'
    argsFilePath = dirname + '/' + name + '.args'

    outputDir = workingDir + '/' + dirname

    if (not path.exists(outputDir)):
        os.makedirs(outputDir)

    errFilePath = outputDir + '/' + name + '.err'
    objFileName = outputDir + '/' + name + '.o'
    binFileName = outputDir + '/' + name


    if path.exists(objFileName):
        os.remove(objFileName);

    if path.exists(binFileName):
        os.remove(binFileName);

    args = []
    if path.exists(argsFilePath):
        with open(argsFilePath) as argsFile:
            for argLine in argsFile:
                args.append(argLine.strip())
    else:
        args.append("")

    err = open(errFilePath, 'w+')
    compialtionCommand = [compiler, "-oneline" , "-o", objFileName, testFilePath]
#    print(compialtionCommand)
    compilation = Popen(compialtionCommand, stdout=sys.stdout, stderr=err)
    exit_code = compilation.wait()

    if path.getsize(errFilePath) > 0:
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"Errors in stderr")
        with open(errFilePath, 'r') as f:
            print(f.read())
        numOfFailedTests = numOfFailedTests + 1
    elif exit_code != 0:
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"  Compilation crashed (exit code {exit_code})")
        numOfFailedTests = numOfFailedTests + 1
    else:
        linkage = Popen(["gcc", objFileName, "-o" , binFileName, "-lm"], stdout=sys.stdout, stderr=sys.stderr)
        exit_code = linkage.wait()
        if exit_code != 0:
            print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
            print(f"  Linkage crashed (exit code {exit_code})")
            numOfFailedTests = numOfFailedTests + 1
        else:
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
                    numOfFailedTests = numOfFailedTests + 1
                else:
                    print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)


def runPPTest(compiler, workingDir, dirname, name):
    global numOfFailedTests
    testFilePath = dirname + '/' + name + '.c'
    expectFilePath = dirname + '/' + name + '.expect'

    outputDir = workingDir + '/' + dirname

    if (not path.exists(outputDir)):
        os.makedirs(outputDir)

    actualFilePath = outputDir + '/' + name + '.actual'

    if path.exists(actualFilePath):
        os.remove(actualFilePath);

    out = open(actualFilePath, 'w+')

    compialtionCommand = [compiler, "-E", testFilePath]
#    print(compialtionCommand)
    compilation = Popen(compialtionCommand, stdout=out, stderr=sys.stderr)
    exit_code = compilation.wait()

    if exit_code != 0:
        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
        print(f"  Compilation crashed (exit code {exit_code})")
        numOfFailedTests = numOfFailedTests + 1
    else:
        testOk = True
        if (path.exists(expectFilePath)):
            testOk = compareFilesLineByLine("preprocessed", testFilePath, actualFilePath, expectFilePath)

        if (testOk):
            print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)

        updateExpectedFromActualIfNeed("preprocessed", actualFilePath, expectFilePath)


def runTestForData(filePath, compiler, workingDir, testMode):
    global numOfFailedTests
    # print(f"processing {filePath}")
    basename = os.path.basename(filePath)
    dirname = os.path.dirname(filePath)
    index_of_dot = basename.index('.')
    suffix = basename[index_of_dot + 1:]
    # print(f"dirname: {dirname}, baseName: {basename}, suffix: {suffix}")
    name = basename[:index_of_dot]
    if (suffix == "c"):
        if (testMode == 'p'):
            runParserTest(compiler, workingDir, dirname, name)
        elif testMode == 'pp':
            runPPTest(compiler, workingDir, dirname, name)
        else:
            runCodegenTest(compiler, workingDir, dirname, name)



def walkDirectory(path, indent, block):
    for file in path.iterdir():
        # print('\t' * indent + f"Walk path {path}")
        if file.is_dir():
            walkDirectory(file, indent + 1, block)
        else:
            block(file)


def main():
    global numOfFailedTests
    arguments = len(sys.argv) - 1
    position = 1
    compiler = ''
    workingDir = ''
    testPaths = []

    testMode = ''

    while (arguments >= position):
        arg = sys.argv[position]
        if arg == '-comp':
            compiler = sys.argv[position + 1]
            position = position + 1
        elif arg == '-workDir':
            workingDir = sys.argv[position + 1]
            position = position + 1
        elif arg == '-p':
            testPaths.append(sys.argv[position + 1])
            position = position + 1
        elif arg == '-m':
            testMode = sys.argv[position + 1];
            position = position + 1
        else:
            print(f"Unknown option \'${arg}\' at position {position:>6}")
        position = position + 1
    if compiler == '':
        print(f"compiler is not specified")
        exit(-1)
    elif workingDir =='':
        print(f"working dir is not specified")
        exit(-1)
    elif not testPaths:
        print(f"no test dir provided")
        exit(-1)
    
    if testMode == '':
        testMode = 'p';

    # print(f"compiler = {compiler}, workind dir = {workingDir}")

    for testPath in testPaths:
        path = Path(testPath)
        if path.exists():
            if path.is_dir():
                walkDirectory(path, 0, lambda a: runTestForData(a, compiler, workingDir, testMode))

    exit (numOfFailedTests)



if __name__ == "__main__":
   main()







