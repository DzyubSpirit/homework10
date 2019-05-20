#include <ctime>
#include <float.h>
#include <iomanip>
#include <iostream>
#include <math.h>
#include <mpi.h>

using namespace std;

#define EPS 1e-4
#define A 0.0
#define B 15.0

double func(double x) { return x * x * x * cos(x); }

void integral(int rank, int np) {
  int n;
  if (rank == 0) {
    cin >> n;
  }
  clock_t start_time = clock();
  MPI_Bcast(&n, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
  cout << setprecision(4 - log10(EPS));
  for (int i = 0; i < n; i++) {
    double prev_sum = DBL_MAX, global_sum;
    long long step_count;
    for (step_count = 1; true; step_count *= 2) {
      double h = (B - A) / step_count;
      double local_sum = 0;
      for (long long i = rank; i < step_count; i += np) {
        local_sum += h * func(A + h * (i + 0.5));
      }
      MPI_Reduce(&local_sum, &global_sum, 1, MPI_DOUBLE, MPI_SUM, 0,
                 MPI_COMM_WORLD);
      int is_enough = fabs(global_sum - prev_sum) / 3 <= EPS;
      MPI_Bcast(&is_enough, 1, MPI_INT, 0, MPI_COMM_WORLD);
      if (is_enough) {
        break;
      }
      prev_sum = global_sum;
    }
    if (i == 0 && rank == 0) {
      cout << "Result: " << setprecision(1 - log10(EPS)) << global_sum << endl;
      cout << "Iteration count: " << step_count * 2 - 1 << endl;
    }
  }
  clock_t end_time = clock();
  if (rank == 0)
    cout << "Time: "
         << double(end_time - start_time) / CLOCKS_PER_SEC / n * 1000 << "ms"
         << endl;
}

int main(int argc, char **argv) {
  MPI_Init(&argc, &argv);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  int np;
  MPI_Comm_size(MPI_COMM_WORLD, &np);

  integral(rank, np);

  MPI_Finalize();
  return 0;
}
