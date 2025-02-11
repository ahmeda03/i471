#!/bin/sh

if [ $# -ne 1 -a $# -ne 2 ]
then
   echo "usage: $0 SHELL_SCRIPT [TEST_FILE_PATH]" 
   exit 1
fi

if [ ! -r $1 ]
then
    echo "cannot find $1"
    exit 1
fi

if [ $# -eq 2 -a ! -e $2 ]
then
    echo "cannot find test file $2"
    exit 1;
fi

RUN_DIR=`dirname $1`;
if [ -z $RUN_DIR ] ; then RUN_DIR=. ; fi

PROG=./`basename $1`

scriptDir=`dirname $0`
testDir=`realpath $scriptDir`/tests
outDir="$HOME/tmp"
mkdir -p $outDir

cd $RUN_DIR

#pwd; echo $RUN_DIR $1 `basename $1` $PROG $testDir $outDir; exit

if [ $# -ne 2 ]
then
   cmpTests=$testDir/*.test
   errTests=$testDir/*.err
elif [ `basename $2 .err` != `basename $2` ]
then
    cmpTests=""
    errTests=$testDir/`basename $2`
else
    cmpTests=$testDir/`basename $2`
    errTests=""
fi

for t in $cmpTests
do
    base=`basename $t .test`
    out="$outDir/$base.out"
    gold=$testDir/$base.out
    sh $PROG < $t | jq -S > $out
    if cmp $gold $out > /dev/null
    then
	echo "`basename $t` ok"
	rm $out
    else
	echo "test $t failed; output in $out"
	echo "run 'diff $gold $out' to see differences"
    fi
done

for t in $errTests
do
    sh $PROG < $t > /dev/null 2>&1
    if [ $? -ne 0 ] 
    then
	echo "`basename $t` ok"
    else
	echo "test $t failed: should have exited with failure"
    fi
done

    

