#include "immintrin.h"
#include <stdint.h>
#include <stdbool.h>

#define BITS 1024

typedef __m256i word1024[4] __attribute__((aligned(64)));

// slow and possibly broken
// #define AVX

void or_word1024(__m256i *a, const __m256i *b) {
  for (int i=0; i < BITS / 256; i++)
    a[i] = _mm256_or_si256(a[i], b[i]);
}

void and_word1024(__m256i *a, const __m256i *b) {
  for (int i=0; i < BITS / 256; i++)
    a[i] = _mm256_and_si256(a[i], b[i]);
}

bool eq_word1024(const __m256i *a, const __m256i *b) {
  uint64_t *aa = (uint64_t *) a;
  uint64_t *bb = (uint64_t *) b;
  bool ret = true;
  for (int i=0; i < BITS / 64; i++) {
    ret &= aa[i] == bb[i];
  }
  return ret;

  //__m256i mask = _mm256_setzero_si256();
  //return
  //    _mm256_testz_si256(_mm256_cmpeq_epi64(a[0], b[0]), mask)
  //  & _mm256_testz_si256(_mm256_cmpeq_epi64(a[1], b[1]), mask)
  //  & _mm256_testz_si256(_mm256_cmpeq_epi64(a[2], b[2]), mask)
  //  & _mm256_testz_si256(_mm256_cmpeq_epi64(a[3], b[3]), mask);
}

#ifdef AVX
double bounded_jaccard_avx(const __m256i *a, const __m256i *b) {
  int num = 0;
  int denom = 0;
  for (int i=0; i < BITS / 256; i++) {
    __m256i _and = _mm256_and_si256(a[i], b[i]);
    for (int j=0; j < 256 / 64; j++) {
      num += _mm_popcnt_u64(_mm256_extract_epi64(_and, j));
      denom += _mm_popcnt_u64(_mm256_extract_epi64(a[i], j));
    }
  }
  return 1. * num / denom;
}
#endif

double bounded_jaccard(const __m256i *a, const __m256i *b) {
  int num = 0;
  int denom = 0;
  for (int i=0; i < BITS / 64; i++) {
    uint64_t ai = ((uint64_t*) a)[i];
    uint64_t bi = ((uint64_t*) b)[i];
    uint64_t _and = ai & bi;
    num += _mm_popcnt_u64(_and);
    denom += _mm_popcnt_u64(ai);
  }
  return 1. * num / denom;
}

#ifdef AVX
double jaccard_avx(const __m256i *a, const __m256i *b) {
  int num = 0;
  int denom = 0;
  for (int i=0; i < BITS / 256; i++) {
    __m256i _or = _mm256_or_si256(a[i], b[i]);
    __m256i _and = _mm256_and_si256(a[i], b[i]);
    for (int j=0; j<N; j++) {
      num += _mm_popcnt_u64(_mm256_extract_epi64(_and, j));
      denom += _mm_popcnt_u64(_mm256_extract_epi64(_or, j));
    }
  }
  return 1. * num / denom;
}
#endif

double jaccard(const __m256i *a, const __m256i *b) {
  int num = 0;
  int denom = 0;
  for (int i=0; i < BITS / 64; i++) {
    uint64_t ai = ((uint64_t*) a)[i];
    uint64_t bi = ((uint64_t*) b)[i];
    uint64_t _or = ai | bi;
    uint64_t _and = ai & bi;
    num += _mm_popcnt_u64(_and);
    denom += _mm_popcnt_u64(_or);
  }
  return 1. * num / denom;
}
