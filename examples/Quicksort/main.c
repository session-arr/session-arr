#include "Quicksort.h"
#include <inttypes.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>

#ifndef REPETITIONS
#define REPETITIONS 1
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
  if (s <= 0) { \
    printf("\tK: seq\n"); \
  } else { \
    printf("\tK: %d\n", s); \
  } \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS <= 1? 0 : sqrt(var / (REPETITIONS - 1))); \
}

#define CLEARCACHE() { \
     const int sze = 20*1024*1024; \
     char *c = (char *)malloc(sze); \
     for (int i = 0; i < 0xffff; i++) \
       for (int j = 0; j < sze; j++) \
         c[j] = i*j; \
     free(c); \
}

#define PRINT(l) { \
  for(int i = 0; i < l.size; i++){ \
    printf("%d ", l.elems[i]); \
  } \
  printf("\n", l.elems[i]); \
}

#define BENCHSTEP(i, f) { \
  start[i] = get_time(); \
  out = f(tmp); \
  end[i] = get_time(); \
  time_diff[i] = end[i] - start[i]; \
  time_old[i] = time[i]; \
  time[i] += (time_diff[i] - time[i])/(i+1); \
  var[i] += (time_diff[i] - time[i]) * (time_diff[i] - time_old[i]); \
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_int_t randvec(size_t s){
  vec_int_t in;
  in.elems = (int *)calloc(s, sizeof(int));
  in.size = s;

  srand(get_time());

  for (int i = 0; i < s; i++) {
    in.elems[i] = (int)rand() % 100;
  }

  return in;
}

pair_vec_int_vec_int_t filter(vec_int_t in){
  pair_vec_int_vec_int_t out;
  size_t i = 0;
  size_t j = in.size - 1;
  int tmp;
  int pivot = in.elems[i];
  do {
    while (i < j && pivot >= in.elems[i]) { i++; }
    while (j >= 0 && pivot < in.elems[j]) { j--; }
    if (i < j) {
      tmp = in.elems[i];
      in.elems[i] = in.elems[j];
      in.elems[j] = tmp;
    }
  } while (i < j);
  tmp = in.elems[0];
  in.elems[0] = in.elems[j];
  in.elems[j] = tmp;
  out.fst = in;
  out.fst.size = j + 1;
  if (out.fst.size == in.size) {
    out.fst.size = in.size / 2;
  }
  out.snd.elems = in.elems + out.fst.size;
  out.snd.size = in.size - out.fst.size;
  return out;
}


vec_int_t cat(pair_vec_int_vec_int_t in){
  in.fst.size += in.snd.size;
  return in.fst;
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

  vec_int_t in, out;
  // Warmup
  // for(int i=0; i<10; i++){
  //   in = randvec(size);
  //   // printf("In %d: ", in.size);
  //   // for (int i = 0; i < in.size; i++) {
  //   //   printf("%d ", in.elems[i]);
  //   // }
  //   //printf("\n");
  //   out = parMsort1(in);
  //   //printf("Out %d: ", out.size);
  //   //for (int i = 0; i < out.size; i++) {
  //   //  printf("%d ", out.elems[i]);
  //   //}
  //   //printf("\n \n");
  //   free(in.elems);
  // }
  double start = 0;
  double end = 0;
  double time = 0;
  double time_diff = 0;
  double time_old = 0;
  double var = 0;

  BENCHMARK(0, parMsort0);
  BENCHMARK(1, parMsort1);
  BENCHMARK(2, parMsort2);
  BENCHMARK(3, parMsort3);
  BENCHMARK(4, parMsort4);
  BENCHMARK(5, parMsort5);
  BENCHMARK(6, parMsort6);
  BENCHMARK(7, parMsort7);
  BENCHMARK(8, parMsort8);
  BENCHMARK(9, parMsort9);
  BENCHMARK(10, parMsort10);


  // double start[9], end[9];
  // double time[] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  // double time_diff[] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  // double time_old[] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
  // double var[] = {0, 0, 0, 0, 0, 0, 0, 0, 0};

  // vec_int_t tmp;

  // for(int i=0; i<REPETITIONS; i++){
  //   in = randvec(size);
  //   tmp.elems = (int *) malloc (size * sizeof(int));
  //   tmp.size = size;

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(0, parMsort0);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(1, parMsort1);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(2, parMsort2);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(3, parMsort3);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(4, parMsort4);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(5, parMsort5);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(6, parMsort6);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(7, parMsort7);

  //   memcpy(tmp.elems, in.elems, size * sizeof(int));
  //   BENCHSTEP(8, parMsort8);

  //   free(in.elems);
  //   free(tmp.elems);
  // }
  // for (int i=0; i<9; i++){
  //   printf("\tK: %d\n", i);
  //   printf("\t\tmean: %f\n", time[i]);
  //   printf("\t\tstddev: %f\n", REPETITIONS <= 1? 0 : sqrt(var[i] / (REPETITIONS - 1)));
  // }
}
