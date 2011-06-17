#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpi.h"

#define func(x) sin(x)

#define ERUSAGE 1

int main (int argc, char * argv[])
{

  double a;
  double b;
  int num_of_iter;

  int num_of_procs;
  int my_rank;
  
  double result = 0.0;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &num_of_procs);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  if (argc != 4) {
    if (my_rank == 0)
      printf("Usage: mpirun -np <NUM> ./simpson <start> <end> <num of iter>\n");
    MPI_Finalize();
    exit(ERUSAGE);
  } else {
    a = atof(argv[1]);
    b = atof(argv[2]);
    num_of_iter = atoi(argv[3]);
    num_of_iter += (num_of_iter % 2);

  }
  
  double h = (b - a) / num_of_iter;

  int local_iter = num_of_iter / num_of_procs;
  int local_idx = my_rank * local_iter;
  int local_end = my_rank * local_iter + local_iter;

  if ((num_of_iter % num_of_procs) != 0) {
    if (my_rank == (num_of_procs - 1))
      local_end += (num_of_iter % num_of_procs);
  }

  printf("proc[%d]: iterating from: %d to: %d\n", my_rank, local_idx, local_end);
  double x;
  double accum = 0.0;

  for ( ; local_idx < local_end; local_idx++) {
    x = a + local_idx * h;
    if ((local_idx % 2) == 0)
      accum += func(x) * 2;
    else
      accum += func(x) * 4;
  }
  
  MPI_Reduce(&accum, &result, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

  if (my_rank == 0) { 
    /* The Masta' Process */
    result += func(b);
    result *= (h/3.0);
    printf("proc[%d]: result: %.20f\n", my_rank, result);
  }

  MPI_Finalize();
  return 0;
}
