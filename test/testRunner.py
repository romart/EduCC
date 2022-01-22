
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

def runTestForData(filePath, compiler, workingDir):
    global numOfFailedTests
    # print(f"processing {filePath}")
    basename = os.path.basename(filePath)
    dirname = os.path.dirname(filePath)
    index_of_dot = basename.index('.')
    suffix = basename[index_of_dot + 1:]
    # print(f"dirname: {dirname}, baseName: {basename}, suffix: {suffix}")
    name = basename[:index_of_dot]
    if (suffix == "c"):
        testFilePath = dirname + '/' + name + '.c'
        expectedFilePath = dirname + '/' + name + '.txt'
        outputDir = workingDir + '/' + dirname

        if (not path.exists(outputDir)):
            os.makedirs(outputDir)

        actualFilePath = workingDir + '/' + expectedFilePath
        # print(f"Run process: {compiler} -astDump {actualFilePath} {testFilePath}")
        process = Popen([compiler, "-astDump", actualFilePath, testFilePath], stdout=PIPE, stderr=DEVNULL)
        exit_code = process.wait()
        if exit_code != 0:
            print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
            print(f"  Process crashed (exit code {exit_code})")
            numOfFailedTests = numOfFailedTests + 1
        else:
            if (path.exists(expectedFilePath)):
                with open(expectedFilePath) as expected, open(actualFilePath) as actual:
                    expt = expected.readlines()
                    actl = actual.readlines()

                    if len(expt) != len(actl):
                        print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
                        print(f"  actual output len ({len(actl)}) differs from expected len ({len(expt)})")
                        numOfFailedTests = numOfFailedTests + 1
                        return
                    else:
                        i = 0
                        
                        while (i < len(expt)):
                            e = expt[i].rstrip()
                            a = actl[i].rstrip()
                            if (e != a):
                                print(CBOLD + CRED + f"Test {testFilePath} -- FAIL" + RESET)
                                print(f"  actual output differs from expected in line {i + 1}")
                                print(f"  ACTUAL:   {a}")
                                print(f"  EXPECTED: {e}")
                                numOfFailedTests = numOfFailedTests + 1
                                return
                            i = i + 1

            print(CBOLD + CGREEN + f"Test {testFilePath} -- OK" + RESET)

            if (not path.exists(expectedFilePath)):
                print("  info: no expected file, create it")
                result = open(actualFilePath).read()
                open(expectedFilePath, 'w+').write(result)



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
    
    # print(f"compiler = {compiler}, workind dir = {workingDir}")

    for testPath in testPaths:
        path = Path(testPath)
        if path.exists():
            if path.is_dir():
                walkDirectory(path, 0, lambda a: runTestForData(a, compiler, workingDir))

    exit (numOfFailedTests)



if __name__ == "__main__":
   main()







