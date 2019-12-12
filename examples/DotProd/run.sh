#!/bin/bash

MAXCORES=$(grep ^cpu\\scores /proc/cpuinfo | uniq |  awk '{print $4}')
NCORES=${1:-"${MAXCORES}"}
NCORES=$((${NCORES}>=${MAXCORES} ? ${MAXCORES} : ${NCORES}))
NAME=t_${NCORES}

MAXSIZE=${2:-"30"}

SIZES=
for i in `seq 9 ${MAXSIZE}`
do
  SIZES="${SIZES} $((2 ** i))"
done

DATA_DIR=${PWD}/data
DATA=${DATA_DIR}/${NAME}
EXE=${PWD}/bench

if [ ! -d "${DATA_DIR}" ]
then
  mkdir ${DATA_DIR}
fi

echo > ${DATA}

CID=$(( ${NCORES} - 1 ))

for SIZE in ${SIZES}
do
  echo "size: ${SIZE}" >> ${DATA}
  echo "# running ${EXE} ${SIZE} on CPUS 0-${CID}"
  numactl -C 0-${CID} ${EXE} ${SIZE} >> ${DATA}
done
