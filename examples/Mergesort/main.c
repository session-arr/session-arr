#include "Mergesort.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#ifndef REPETITIONS
#define REPETITIONS 50
#endif

#define BENCHMARK(s, f) { \
  time = 0; \
  time_diff = 0; \
  time_old = 0; \
  var = 0; \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(size); \
    start = get_time(); \
    out = f(in); \
    end = get_time(); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
    free(in.elems); \
  } \
  printf("\tK: %s\n", s); \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1))); \
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

  srand(get_time());

  for (int i = 0; i < s; i++) {
    in.elems[i] = (double)rand();
  }

  return in;
}

vec_double_t merge(pair_int_pair_vec_double_vec_double_t in){
  vec_double_t out;
  int z=0;
  int i=0;
  int j=0;
  int *tmp = (int *)malloc(in.fst * sizeof(int));
  while (i < in.snd.fst.size && j < in.snd.snd.size) {
      if (in.snd.fst.elems[i] <= in.snd.snd.elems[j]) {
        tmp[z++] = in.snd.fst.elems[i++];
      } else {
        tmp[z++] = in.snd.snd.elems[j++];
      }
  }
  if (i < in.snd.fst.size) {
    memcpy(tmp + z, in.snd.fst.elems + i, sizeof(int) * (in.fst - z));
  }
  if (j < in.snd.snd.size) {
    memcpy(tmp + z, in.snd.snd.elems + j, sizeof(int) * (in.fst - z));
  }
  out.size = in.fst;
  out.elems = in.snd.fst.elems;
  memcpy(out.elems, tmp, in.fst * sizeof(int));
  free(tmp);
  return out;
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

  vec_double_t in, out;
  double start, end;
  // Warmup
  for(int i=0; i<10; i++){
    in = randvec(size);
    out = parMsort3(in);
//  for (int i = 0; i < out.size; i++) {
//    printf("%d ", out.elems[i]);
//  }
   //printf("%d\n\n", i);
    free(in.elems);
  }


  double time = 0;
  double time_diff = 0;
  double time_old = 0;
  double var = 0;
  BENCHMARK("seq", parMsort0)
  BENCHMARK("1", parMsort1)
  //BENCHMARK("ms1a", parMsort1a)
  BENCHMARK("2", parMsort2)
  BENCHMARK("3", parMsort3)
  BENCHMARK("4", parMsort4)
  BENCHMARK("5", parMsort5)
  BENCHMARK("6", parMsort6)
  BENCHMARK("7", parMsort7)
  BENCHMARK("8", parMsort8)

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
