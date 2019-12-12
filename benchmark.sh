#!/bin/bash

me=`basename "$0"`

sbuild () {
  echo "+ stack exec -- session-arrc $1.hs"
  stack exec -- session-arrc $1.hs
  echo "+ gcc $1.c main.c -DREPETITIONS=${REPETITIONS} -pthread -lm -o bench"
  gcc $1.c main.c -DREPETITIONS=${REPETITIONS} -pthread -lm -o bench
}

MAXCORES=$(grep ^cpu\\scores /proc/cpuinfo | uniq |  awk '{print $4}')
NCORES=${CORES:-"${MAXCORES}"}
NCORES=$((${NCORES}>=${MAXCORES} ? ${MAXCORES} : ${NCORES}))

REPETITIONS=${REPETITIONS:-"50"}
MAXSIZE=${MAXSIZE:-"30"}
MAXSIZE=$((${MAXSIZE}<15 ? 15 : ${MAXSIZE}))

echo "# Running CORES=${NCORES} REPETITIONS=${REPETITIONS} MAXSIZE=${MAXSIZE} ./${me}"

DIRS="DotProd FFT Mergesort Quicksort ScalarMulMat"

pushd ./examples > /dev/null
echo "+ pushd ./examples"

for dir in ${DIRS}
do
  pushd ./${dir} > /dev/null
  echo
  echo "+ pushd ./${dir}"
  GT=$(sbuild ${dir} | tee /dev/tty | sed -n 5p)
  echo "# Generating global type for: ${GT}"
  echo "+ stack exec -- session-arrc --infer ${GT} ${dir}.hs"
  stack exec -- session-arrc --infer ${GT} ${dir}.hs
  cat ${dir}_${GT}.mpst
  ./run.sh ${NCORES} ${MAXSIZE}
  popd > /dev/null
  echo "+ popd"
  echo
  echo "# Plotting results"
  echo "+ ./plotall.sh ${dir} ${NCORES}"
  ./plotall.sh ${dir} ${NCORES}
  echo
done

popd > /dev/null
echo "+ popd"
popd > /dev/null
