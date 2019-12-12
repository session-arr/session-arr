#!/bin/bash

MAXCORES=$1
NCORES=$2
NAME=t_${MAXCORES}_${NCORES}

SIZES=
for i in `seq 4 9`
do
  SIZES="${SIZES} $((2 ** i * 10 ** 6))"
done

REPLACEMENTS="\
s/\${MAXCORES}/${MAXCORES}/g;\
s/\${NCORES}/${NCORES}/g;\
s/\${NAME}/${NAME}/g;\
s/\${SIZES}/${SIZES}/g" 

# qsub -N ${NAME} -
sed -e "${REPLACEMENTS}"  << 'EOF' | qsub -N ${NAME} -
#!/bin/bash
#PBS -lwalltime=24:00:00
#PBS -lselect=1:ncpus=${MAXCORES}:mpiprocs=1:ompthreads=${MAXCORES}:mem=62gb

DATA_DIR=${PBS_O_WORKDIR}/data
DATA=${DATA_DIR}/${NAME}
EXE=${PBS_O_WORKDIR}/bench

if [ ! -d "${DATA_DIR}" ]
then
  mkdir ${DATA_DIR}
fi

echo > ${DATA}

cat /proc/cpuinfo
${PBS_O_WORKDIR}/cpuinfo.sh
numactl -H
lscpu
echo "Using ${NCORES}, no hyperthreading:"
NCORES=$(( ${NCORES} - 1 ))
numactl --physcpubind=0-${NCORES} numactl --show

for SIZE in ${SIZES}
do
  echo "size: ${SIZE}" >> ${DATA}
  numactl --physcpubind=0-${NCORES} ${EXE} ${SIZE} >> ${DATA}
done
EOF
