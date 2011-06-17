CC=mpicc
SRCS=simpsons_rule_mpi.c
all: mpisimpson

mpisimpson:
	$(CC) -Wall -lm -o mpisimpson $(SRCS)

clean:
	-rm mpisimpson *.o

