#include <mpi.h>
#include <iostream>

using namespace std;

#define TAG_X 0
#define TAG_PART 1

double nth(double x, int n) {
  double res = n & 1 == 1 ? -1 : 1;
  for (int i = 1; i <= n; i++) {
    res *= (x-1) * (n + i) / i / 4;
  }
  res /= (1 - 2 * n);
  return res;
}

void master_thread(int rank, int np) {
  double x;
  cout << "Write x:" << endl;
  cin >> x;
  bool inv = false;
  if (x >= 2) {
    inv = true;
    x = 1 / x;
  }
  double e;
  cout << "Write e:" << endl;
  cin >> e;

  for (int i = 1; i < np; i++) {
    double dat[2] = { x, e };
    MPI_Send(dat, 2, MPI_DOUBLE, i, TAG_X, MPI_COMM_WORLD);
  }

  double res = 1;
  bool has_zero = false;
  int i = 0;
  while (!has_zero) {
    for (int j = 1; j < np; j++) {
      double part;
//      cout << "r 1 " << j << endl;
      MPI_Recv(&part, 1, MPI_DOUBLE, j, TAG_PART, MPI_COMM_WORLD,
          MPI_STATUS_IGNORE);
      if (part < e) has_zero = true;
      res += part;
    }
    res += nth(x, i * np + 1);
//    cout << i << " " << np << endl;
//    cout << "part " << np * i + 1 << " is " << nth(x, i * np + 1) << endl;
    i++;
  }
  if (inv) res = 1 / res;
  cout << res << endl;
}

void worker_thread(int rank, int np) {
  double dat[2];
  MPI_Recv(dat, 2, MPI_DOUBLE, 0, TAG_X, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

  double x = dat[0], e = dat[1];
  int i = 0;
  double part = nth(x, np * i + rank + 1);
//  cout << "part " << np * i + rank + 1 << " of " << x << " is " << part << endl;
  while (part >= e) {
//    cout << "s " << rank << endl;
    MPI_Send(&part, 1, MPI_DOUBLE, 0, TAG_PART, MPI_COMM_WORLD);
    i++;
    part = nth(x, np * i + rank);
//    cout << "part " << np * i + rank << " is " << part << endl;
  }
  part = 0;
  MPI_Send(&part, 1, MPI_DOUBLE, 0, TAG_PART, MPI_COMM_WORLD);
//  cout << "s " << rank << endl;
//  cout << rank << " done" << endl;
}

int main(int argc, char** argv) {
  MPI_Init(&argc, &argv);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  int np;
  MPI_Comm_size(MPI_COMM_WORLD, &np);

  int x;
  if (rank == 0) {
    master_thread(rank, np);
  } else {
    worker_thread(rank, np);
  }

  MPI_Finalize();
  return 0;
}
