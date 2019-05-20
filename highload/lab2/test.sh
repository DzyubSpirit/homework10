mpic++ main.cc
for i in 1 2 3 4 5 6 7 8
do
  echo "10000" | mpiexec -hostfile hostfile -n $i a.out
done
