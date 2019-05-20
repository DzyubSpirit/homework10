#include <ctime>
#include <fstream>
#include <mpi.h>

using namespace std;

#define Y_TAG 0
#define X_TAG 1

void solve(int np, int rank) {
  int n, iter_count;
  ifstream in;
  if (rank == 0) {
    string filename;
    cin >> filename;
    cin >> iter_count;
    in = ifstream(filename);
    in >> n;
  }
  MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
  double *mag, bg[n];
  if (rank == 0) {
    mag = new double[n * n];
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        in >> mag[j * n + i];
      }
      in >> bg[i];
    }
    in.close();
  }
  cout << rank << " " << iter_count << endl;
  MPI_Bcast(&iter_count, 1, MPI_INT, 0, MPI_COMM_WORLD);
  clock_t start_time = clock();
  double ma[n * n], b[n];
  for (int iter_i = 0; iter_i < iter_count; iter_i++) {
    if (rank == 0) {
      for (int i = 0; i < n * n; i++) {
        ma[i] = mag[i];
      }
      for (int i = 0; i < n; i++) {
        b[i] = bg[i];
      }
    }
    int h = n / np;
    double mah[h * n];
    if (rank == 0) {
      MPI_Scatter(ma, n * h, MPI_DOUBLE, mah, n * h, MPI_DOUBLE, 0,
                  MPI_COMM_WORLD);
    } else {
      MPI_Scatter(NULL, 0, MPI_DATATYPE_NULL, mah, n * h, MPI_DOUBLE, 0,
                  MPI_COMM_WORLD);
    }
    double coefs[n];
    for (int i = 0; i < n - 1; i++) {
      if (i / h == rank) {
        for (int j = i + 1; j < n; j++) {
          coefs[j] = mah[(i % h) * n + j] / mah[(i % h) * n + i];
        }
      }
      MPI_Bcast(coefs + i + 1, n - 1 - i, MPI_DOUBLE, i / h, MPI_COMM_WORLD);
      for (int j = rank * h; j < (rank + 1) * h; j++) {
        if (j < i)
          continue;
        if (i == j) {
          for (int k = i + 1; k < n; k++) {
            mah[(j % h) * n + k] = coefs[k];
          }
          continue;
        }
        for (int k = i + 1; k < n; k++) {
          mah[(j % h) * n + k] -= mah[(j % h) * n + i] * coefs[k];
        }
      }
    }
    if (rank != 0) {
      MPI_Recv(b, n, MPI_DOUBLE, rank - 1, Y_TAG, MPI_COMM_WORLD, NULL);
    }
    for (int i = rank * h; i < (rank + 1) * h; i++) {
      for (int j = i + 1; j < n; j++) {
        b[j] -= b[i] * mah[(i % h) * n + j];
      }
    }
    if (rank != np - 1) {
      MPI_Send(b, n, MPI_DOUBLE, rank + 1, Y_TAG, MPI_COMM_WORLD);
      MPI_Recv(b, n, MPI_DOUBLE, rank + 1, X_TAG, MPI_COMM_WORLD, NULL);
    }
    for (int i = (rank + 1) * h - 1; i >= rank * h; i--) {
      b[i] /= mah[(i % h) * n + i];
      for (int j = i - 1; j >= 0; j--) {
        b[j] -= b[i] * mah[(i % h) * n + j];
      }
    }
    if (rank != 0) {
      MPI_Send(b, n, MPI_DOUBLE, rank - 1, X_TAG, MPI_COMM_WORLD);
    }
    if (rank == 0 && iter_i == iter_count - 1) {
      clock_t end_time = clock();
      // for (int i = 0; i < n; i++) {
      //  cout << b[i] << " ";
      // }
      // cout << endl;
      cout << double(end_time - start_time) / iter_count / CLOCKS_PER_SEC << "s"
           << endl;
    }
  }
  delete[] mag;
}

int main(int argc, char **argv) {
  MPI_Init(&argc, &argv);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  int np;
  MPI_Comm_size(MPI_COMM_WORLD, &np);

  solve(np, rank);

  MPI_Finalize();
  return 0;
}
