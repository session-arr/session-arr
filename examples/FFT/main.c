#include "FFT.h"
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
    in = randvec(s, size); \
    start = get_time(); \
    out = f(in); \
    end = get_time(); \
    free_mat(in); \
    time_diff = end - start; \
    time_old = time; \
    time += (time_diff - time)/(i+1); \
    var += (time_diff - time) * (time_diff - time_old); \
  } \
  if (s < 0) { \
    printf("\tK: seq\n"); \
  } else { \
    printf("\tK: %d\n", s); \
  } \
  printf("\t\tmean: %f\n", time); \
  printf("\t\tstddev: %f\n", REPETITIONS<=1? 0: sqrt(var / (REPETITIONS - 1))); \
}

#define WARMUP(f) { \
  for(int i=0; i<REPETITIONS; i++){ \
    in = randvec(0, size); \
    out = f(in); \
    free_mat(in); \
  } \
}

double PI = atan2(1, 1) * 4;

int num_stages;
int num_workers;
vec_cplx_t **stages;

vec_cplx_t zip_add(pair_pair_int_int_pair_vec_cplx_vec_cplx_t in){
  int lvl = in.fst.fst;
  int wid = in.fst.snd;
  vec_cplx_t l = in.snd.fst;
  vec_cplx_t r = in.snd.snd;
  vec_cplx_t lout = stages[lvl][wid];
  for(int i = 0; i < l.size; i++){
    lout.elems[i] = l.elems[i] + r.elems[i];
  }
  return lout;
}

vec_cplx_t zip_sub(pair_pair_int_int_pair_vec_cplx_vec_cplx_t in){
  int lvl = in.fst.fst;
  int wid = in.fst.snd;
  vec_cplx_t l = in.snd.fst;
  vec_cplx_t r = in.snd.snd;
  vec_cplx_t lout = stages[lvl][wid];
  for(int i = 0; i < l.size; i++){
    lout.elems[i] = l.elems[i] - r.elems[i];
  }
  return lout;
}


vec_cplx_t cat(pair_vec_cplx_vec_cplx_t in){
  in.fst.size *= 2;
  return in.fst;
}

void _fft(cplx_t buf[], cplx_t out[], int n, int step)
{
  if (step < n) {
    _fft(out, buf, n, step * 2);
    _fft(out + step, buf + step, n, step * 2);

    for (int i = 0; i < n; i += 2 * step) {
      cplx_t t = cexp(-I * PI * i / n) * out[i + step];
      buf[i / 2]     = out[i] + t;
      buf[(i + n)/2] = out[i] - t;
    }
  }
}

void show(const char * s, vec_cplx_t in) {
	printf("%s", s);
	for (int i = 0; i < in.size; i++)
		if (!cimag(in.elems[i]))
			printf("%g ", creal(in.elems[i]));
		else
			printf("(%g, %g) ", creal(in.elems[i]), cimag(in.elems[i]));
    printf("\n");
}

pair_vec_cplx_vec_cplx_t deinterleave(pair_int_int_t iin){
  int wl = iin.fst;
  int wr = iin.snd;

  int mid = stages[0][wl].size/2;

  stages[1][wl].size = mid;
  stages[1][wr].size = mid;
  stages[1][wr].elems = stages[1][wl].elems + mid;

  for(int i = 0; i < stages[0][wl].size; i+= 2){
    stages[1][wl].elems[i/2] = stages[0][wl].elems[i];
    stages[1][wr].elems[i/2] = stages[0][wl].elems[i+1];
  }
  memcpy(stages[0][wl].elems, stages[1][wl].elems, stages[0][wl].size * sizeof(cplx_t));
  stages[0][wr].elems = stages[0][wl].elems + mid;
  stages[0][wr].size = mid;

  for (int i = 2; i < num_stages; i++){
    memcpy(stages[i][wl].elems, stages[1][wl].elems, stages[0][wl].size * sizeof(cplx_t));
    stages[i][wl].size = mid;
    stages[i][wr].elems = stages[i][wl].elems + mid;
    stages[i][wr].size = mid;
  }
  stages[0][wl].size = mid;
  return (pair_vec_cplx_vec_cplx_t) { stages[0][wl], stages[0][wr] };
}

/*
pair_pair_vec_cplx_vec_cplx_pair_vec_cplx_vec_cplx_t deinterleave(pair_vec_cplx_vec_cplx_t outin){
  vec_cplx_t out = outin.fst;
  vec_cplx_t in = outin.snd;
  int mid = in.size/2;
  for (int i = 0; i < in.size; i+=2){
    out.elems[i/2] = in.elems[i]; // Evens
    out.elems[i/2 + mid] = in.elems[i+1]; //Odds
  }
  memcpy(in.elems, out.elems, in.size * sizeof(cplx_t));
  pair_pair_vec_cplx_vec_cplx_pair_vec_cplx_vec_cplx_t inout;
  pair_vec_cplx_vec_cplx_t evens, odds;
  evens.fst.elems = out.elems;
  evens.fst.size = mid;
  evens.snd.elems = in.elems;
  evens.snd.size = mid;
  odds.fst.elems = out.elems + mid;
  odds.fst.size = mid;
  odds.snd.elems = in.elems + mid;
  odds.snd.size = mid;
  inout.fst = evens;
  inout.snd = odds;
  return inout;
}
*/

void showstep(int stp, const char * s, vec_cplx_t in) {
	printf("%s", s);
	for (int i = 0; i < in.size; i+=stp)
		if (!cimag(in.elems[i]))
			printf("%g ", creal(in.elems[i]));
		else
			printf("(%g, %g) ", creal(in.elems[i]), cimag(in.elems[i]));
    printf("\n");
}

vec_cplx_t baseFFT(pair_int_pair_int_vec_cplx_t in)
{
    int lvl = in.fst;
    int wid = in.snd.fst;
    cplx_t *buf = stages[lvl][wid].elems;
    int n = in.snd.snd.size;

	_fft(buf, in.snd.snd.elems, n, 1);

    return stages[lvl][wid];
}

vec_cplx_t seqfft(vec_cplx_t in)
{
  pair_int_pair_int_vec_cplx_t i = {1, {0, in}};
  return baseFFT(i);
}


vec_cplx_t map_exp(pair_int_pair_int_vec_cplx_t iv){
  int i = iv.snd.fst;
  int ps2x = iv.fst;
  vec_cplx_t in = iv.snd.snd;
  int step = i * in.size;
  for(int k = 0; k < in.size; k++){
     in.elems[k] = in.elems[k] * cexp (2 * - I * PI * (k + step) / (ps2x * in.size));
  }
  return in;
}

void free_mat(vec_cplx_t v){
  for(int i = 0; i < num_stages; i++){
    free(stages[i][0].elems);
    free(stages[i]);
  }
  free(stages);
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}

vec_cplx_t randvec(int depth, size_t s){
  num_workers = depth <= 1? 1 : 1 << depth - 1;
  num_stages = depth <= 1? 2 : 1 + depth ;
  stages = (vec_cplx_t **)malloc(num_stages * sizeof(vec_cplx_t *));
  for (int i = 0; i < num_stages; i++){
    stages[i] = (vec_cplx_t *)malloc(num_workers * sizeof(vec_cplx_t));
    stages[i][0].elems = (cplx_t *)calloc(s, sizeof(cplx_t));
  }
  stages[0][0].size = s;

  srand(time(NULL));


  for (int i = 0; i < s; i++) {
    double rand_r = (double)rand() / (double)RAND_MAX;
    double rand_i = (double)rand() / (double)RAND_MAX;
    stages[0][0].elems[i] = rand_r + rand_i * I;
  }

  for(int j = 1; j < num_stages; j++) {
      memcpy(stages[j][0].elems, stages[0][0].elems, s * sizeof(vec_cplx_t));
      stages[j][0].size = s / num_workers;
  }

  for(int i = 0; i < num_stages; i++) {
    for(int j = 1; j < num_workers; j++) {
      stages[i][j] = stages[i][j-1];
    }
  }

  return stages[0][0];
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
  setbuf(stdout, NULL);
  if (argc <= 1) {
    usage(argv[0]);
  }
  char *endptr = NULL;
  errno = 0;
  size_t size = strtoimax(argv[1],&endptr,10);
  size = (size_t) 1 << (long)ceil(log2(size));
  size = size < 256? 256:size;
  if (errno != 0) {
    printf("%s", strerror(errno));
    usage(argv[0]);
  }
  if (endptr != NULL && *endptr != 0) {
    usage(argv[0]);
  }


  // /*
  // // TEST
  // {
  //   // cplx_t i[] = {1,  1,  1,  1, 1,  1, 0,  0 };
  //   // cplx_t o[] = {1,  1,  1,  1, 1,  1, 0,  0 };
  //   // pair_vec_cplx_vec_cplx_t iin = {{i, 8}, {o, 8}};
  //   // cplx_t i[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0};
  //   // cplx_t o[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0};
  //   // pair_vec_cplx_vec_cplx_t iin = {{i, 16}, {o, 16}};
  //   cplx_t i[] = {1,  1,  1,  1, 0,  0, 0,  0 };
  //   cplx_t o[] = {1,  1,  1,  1, 0,  0, 0,  0 };
  //   pair_vec_cplx_vec_cplx_t iin = {{i, 8}, {o, 8}};

  //   out = baseFFT(iin);
  //   show("Out-fst:", out.fst); printf("\n");
  //   show("Out-snd:", out.snd); printf("\n");
  // }
  // */
  // {
  //   vec_cplx_t in = randvec(0, 64);
  //   // cplx_t i[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0};
  //   // cplx_t o[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0};
  //   // pair_vec_cplx_vec_cplx_t iin = {{i, 16}, {o, 16}};

  //   out = seqfft(in);
  //   // show("Out-fst:", out.fst); printf("\n");
  //   show("Out-snd:", out); printf("\n");
  //   free_mat(in);
  // }
  //

  vec_cplx_t in, out;
  double start, end, time, time_diff, time_old, var;

  WARMUP(seqfft)
  BENCHMARKSEQ(-1, seqfft)
  BENCHMARKSEQ(0, fft0)
  BENCHMARKSEQ(1, fft1)
  BENCHMARKSEQ(2, fft2)
  BENCHMARKSEQ(3, fft3)
  BENCHMARKSEQ(4, fft4)
  BENCHMARKSEQ(5, fft5)
  BENCHMARKSEQ(6, fft6)
  BENCHMARKSEQ(7, fft7)
  BENCHMARKSEQ(8, fft8)
}
