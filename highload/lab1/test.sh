for i in 4 8 9 10 11 12
do
  for ch in 100000000
  do
    echo $ch | mpiexec -hostfile hostfile -n $i a.out
  done
done
