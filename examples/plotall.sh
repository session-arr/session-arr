#!/bin/bash

plotfn() {
  SZ=$1
  NAME=${DIR,,}

  PATH=${DIR}/data/t_${1}
  OUT="${NAME}_$SZ"

  echo "Plotting ${PATH} as ${OUT}_[s|k].pdf"
  ./plot.py ${1} plots/${OUT} ${PATH}
}

[ ! -d ./plots ] && mkdir plots

for d in DotProd FFT Mergesort
do
  DIR=${d}
  plotfn ${1}
done

