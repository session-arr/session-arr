#include "DotProd.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#ifndef REPETITIONS
#define REPETITIONS 50
#endif

#define BENCHMARKSEQ(s, f) { \
  time = 0; \
  time_diff = 0; \
  time_old = 0; \
  var = 0; \
  for(int i=0; i<REPETITIONS; i++){ \
    in.fst = randvec(size); \
    in.snd = randvec(size); \
    start = get_time(); \
    out = f(in); \
    end = get_time(); \
    free_mat(in.fst); \
    free_mat(in.snd); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1))); \
}

#define WARMUP(f) { \
  for(int i=0; i<REPETITIONS; i++){ \
    in.fst = randvec(size); \
    in.snd = randvec(size); \
    out = f(in); \
    free_mat(in.fst); \
    free_mat(in.snd); \
  } \
}


#define BENCHMARKPAR(s, fi, f) { \
  time = 0; \
  time_diff = 0; \
  time_old = 0; \
  var = 0; \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(size); \
    fi(); \
    start = get_time(); \
    out = f(in); \
    end = get_time(); \
    free_mat(in); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1))); \
}

void free_mat(vec_double_t v){
  free(v.elems);
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_double_t randvec(size_t s){
  vec_double_t in;
  in.elems = (double *)calloc(s, sizeof(double));
  in.size = s;

  srand(time(NULL));

  for (int i = 0; i < s; i++) {
    in.elems[i] = (double)rand() / (double)RAND_MAX ;
  }

  return in;
}

double dot(pair_vec_double_vec_double_t v){
  double acc=0;
  for(int i =0 ; i < v.fst.size && i < v.snd.size; i++) {
    acc += v.fst.elems[i] * v.snd.elems[i];
  }
  return acc;
}


void usage(const char *nm){
  printf("Usage: %s <input_size>\n", nm);
  exit(-1);
}


long tspec2ms(struct timespec *tv)
{
	return tv->tv_sec * 1.0e9 + tv->tv_nsec;
}

int main(int argc, const char *argv[]) {
  if (argc <= 1) {
    usage(argv[0]);
  }
  char *endptr = NULL;
  errno = 0;
  size_t size = strtoimax(argv[1],&endptr,10);
  if (errno != 0) {
    printf("%s", strerror(errno));
    usage(argv[0]);
  }
  if (endptr != NULL && *endptr != 0) {
    usage(argv[0]);
  }


  pair_vec_double_vec_double_t in;
  double out;
  double start, end, time, time_diff, time_old, var;

  WARMUP(dotProd32)

  BENCHMARKSEQ("seq", dot)
  BENCHMARKSEQ("1", dotProd1)
  BENCHMARKSEQ("2", dotProd2)
  BENCHMARKSEQ("4", dotProd4)
  BENCHMARKSEQ("8", dotProd8)
  BENCHMARKSEQ("16", dotProd16)
  BENCHMARKSEQ("24", dotProd24)
  BENCHMARKSEQ("32", dotProd32)
  // BENCHMARKSEQ("ms0", parProd)

  // BENCHMARKPAR("ms0", scalarProdInit, scalarProd)

  // time = 0;
  // for(int i=0; i<REPETITIONS; i++){
    // in = randvec(size);
    // clock_gettime(CLOCK_MONOTONIC, &start);
    // out = parMsort1(in);
    // clock_gettime(CLOCK_MONOTONIC, &end);
    // time += (end - start)/REPETITIONS;
    // free(in.elems);
  // }
  // printf("ms1: %d\n", time);

//#ifdef DEBUG
//    if (start.tv_nsec > end.tv_nsec ){
//  for (int i = 0; i < out.size; i++) {
//    printf("%d ", out.elems[i]);
//  }
//  printf("\n");
//    }
//#endif
}
