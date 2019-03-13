#include <ctime>
#include <iomanip>
#include <iostream>
#include <limits.h>
#include <math.h>
#include <mpi.h>

using namespace std;

#define TAG_X 0
#define TAG_PART 1
#define EPS 1e-11
#define CALC_ITER_COUNT 1e2

double next_element(double prev, double x, int n) {
  return -prev * (x - 1) * (2 * n + 1) * (2 * n + 2) * (1 - 2 * n) /
         (1 - 2 * (n + 1)) / (n + 1) / (n + 1) / 4;
}

double nth(double x, double prev_elem, int prev_n, int n) {
  for (int i = prev_n; i < n; i++) {
    prev_elem = next_element(prev_elem, x, i);
  }
  return prev_elem;
}

array<double, 2> part_sum(double x, int rank, int np) {
  int i = 0;
  double elem = nth(x, 1, 0, rank + 1);
  double part = 0;
  // cout << rank << " " << part << endl;
  while (abs(elem) >= EPS) {
    part += elem;
    // cout << rank << " " << part << endl;
    elem = nth(x, elem, np * i + rank + 1, np * (i + 1) + rank + 1);
    i++;
  }
  return {part, double(i)};
}

void master_thread(int rank, int np, int calc_iter_count) {
  double x;
  cout << "Write x:" << endl;
  cin >> x;
  bool inv = false;
  if (x > 1) {
    inv = true;
    x = 1 / x;
  }

  clock_t start_time = clock();
  for (int calc_i = 0; calc_i < calc_iter_count; calc_i++) {
    for (int i = 1; i < np; i++) {
      MPI_Send(&x, 1, MPI_DOUBLE, i, TAG_X, MPI_COMM_WORLD);
    }

    array<double, 2> part_res = part_sum(x, 0, np);
    double res = 1 + part_res[0];
    int iter_count = 1 + int(part_res[1]);
    for (int i = 1; i < np; i++) {
      double part_calc[2];
      MPI_Recv(&part_calc, 2, MPI_DOUBLE, i, TAG_PART, MPI_COMM_WORLD,
               MPI_STATUS_IGNORE);
      iter_count += int(part_calc[1]);
      double recv_part = part_calc[0];
      res += recv_part;
    }
    if (inv)
      res = 1 / res;
    if (calc_i == 0) {
      cout << setprecision(1 - log10(EPS)) << res << endl;
      cout << "Elements count: " << iter_count << endl;
    }
  }
  clock_t end_time = clock();
  double passed_time = double(end_time - start_time) / CLOCKS_PER_SEC;
  cout << "Spent " << passed_time << "s"
       << " on " << calc_iter_count << " iterations." << endl;
  cout << "Average time: " << passed_time / calc_iter_count * 1e6 << "μs"
       << endl;
}

void worker_thread(int rank, int np, int calc_iter_count) {
  for (int calc_i = 0; calc_i < calc_iter_count; calc_i++) {
    double x;
    MPI_Recv(&x, 1, MPI_DOUBLE, 0, TAG_X, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    array<double, 2> part_calc = part_sum(x, rank, np);
    MPI_Send(part_calc.data(), 2, MPI_DOUBLE, 0, TAG_PART, MPI_COMM_WORLD);
  }
}

int main(int argc, char **argv) {
  MPI_Init(&argc, &argv);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  int np;
  MPI_Comm_size(MPI_COMM_WORLD, &np);

  if (rank == 0) {
    master_thread(rank, np, CALC_ITER_COUNT);
  } else {
    worker_thread(rank, np, CALC_ITER_COUNT);
  }

  MPI_Finalize();
  return 0;
}
