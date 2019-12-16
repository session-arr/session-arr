#include "Tutorial.h"

int max(pair_int_int_t x){
  return (x.fst < x.snd ? x.snd : x.fst);
}

int head (vec_int_t x){
  return (x.elems[0]);
}


int main(){
  int test_data[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vec_int_t in = { test_data, 10 };

  printf("The maximum is: %d\n", maxL(in));
}
