#!/bin/bash

for i in `seq 4 9`
do
  SIZES="$((10 ** i)) ${SIZES}"
done

LIST_CORES="$(seq 32 -2 2) 1"
for CORES in ${LIST_CORES}
do

echo "----------- START task_${CORES} ---------------"
echo

# | qsub -Wblock=true -N task_${CORES} -
sed -e "s/\${CORES}/${CORES}/g;s/\${SIZES}/${SIZES}/g" << 'EOF' | qsub -Wblock=true -N task_${CORES} -
#!/bin/bash
#PBS -lwalltime=24:00:00
#PBS -lselect=1:ncpus=32:mpiprocs=1:ompthreads=32:mem=62gb

DATA_DIR=${PBS_O_WORKDIR}/${CORES}
DATA=${DATA_DIR}/time.data
CPUINFO=${DATA_DIR}/cpu.data
EXE=${PBS_O_WORKDIR}/bench

if [ -d "${DATA_DIR}" ]
then
  [ -d "${DATA_DIR}_old" ] && rm -r ${DATA_DIR}_old
  mv ${DATA_DIR} ${DATA_DIR}_old
fi

mkdir ${DATA_DIR}

cat /proc/cpuinfo > ${CPUINFO}
echo >> ${CPUINFO}

for SIZE in ${SIZES}
do
  echo "size: ${i}" >> ${DATA}
  taskset --cpu-list 0-$((${CORES} - 1)) ${EXE} ${SIZE} >> ${DATA}
done
EOF

echo "-------------- task_${CORES}.e* ---------------"
cat task_${CORES}.e* 2>/dev/null
echo

echo "-------------- task_${CORES}.o* ---------------"
cat task_${CORES}.o* 2>/dev/null
echo
rm -f task_${CORES}.e*
rm -f task_${CORES}.o*
echo "------------- END task_${CORES} ---------------"
echo

done
