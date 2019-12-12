#include "ScalarMulMat.h"
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
    in = randvec(size); \
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

void free_mat(vec_vec_double_t v){
  for(int i=0; i<v.size; i++){
    free(v.elems[i].elems);
  }
  free(v.elems);
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_vec_double_t randvec(size_t s){
  vec_vec_double_t in;
  in.elems = (vec_double_t *)calloc(s, sizeof(vec_double_t));
  in.size = s;

  srand(time(NULL));

  for (int i = 0; i < s; i++) {
    in.elems[i].size = s;
    in.elems[i].elems = (double *)calloc(s, sizeof(double));
    for(int j = 0; j < s; j++){
      in.elems[i].elems[j] = (double)rand();
    }
  }

  return in;
}

vec_vec_double_t cat(pair_vec_vec_double_vec_vec_double_t in){
  in.fst.size += in.snd.size;
  return in.fst;
}

vec_vec_double_t prod(vec_vec_double_t v){
  for(int i = 0; i < v.size; i++){
    for (int j = 0; j < v.elems[i].size; j++){
      v.elems[i].elems[j] *= sqrt(v.elems[i].elems[j]);
    }
  }
  return v;
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


  vec_vec_double_t in, out;
  double start, end, time, time_diff, time_old, var;
  // Warmup
  for(int i=0; i<REPETITIONS; i++){
    in = randvec(size);
    out = prod(in);
    free_mat(in);
  }

  BENCHMARKSEQ("seq", prod)
  BENCHMARKSEQ("1" , parProd1)
  BENCHMARKSEQ("2" , parProd2)
  BENCHMARKSEQ("3" , parProd3)
  BENCHMARKSEQ("4" , parProd4)
  BENCHMARKSEQ("8" , parProd8)
  BENCHMARKSEQ("16", parProd16)
  BENCHMARKSEQ("32", parProd32)
  BENCHMARKSEQ("64", parProd64)

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
