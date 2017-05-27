#include "immintrin.h"
#include <stdint.h>
#include <stdbool.h>

#define N 4
// #define AVX

void or_word1024(__m256i *a, const __m256i *b) {
  for (int i=0; i<N; i++)
    a[0] = _mm256_or_si256(a[i], b[i]);
}

void and_word1024(__m256i *a, const __m256i *b) {
  for (int i=0; i<N; i++)
    a[0] = _mm256_and_si256(a[i], b[i]);
}

bool eq_word1024(__m256i *a, __m256i *b) {
  __m256i mask = _mm256_setzero_si256();
  return
      _mm256_testz_si256(_mm256_cmpeq_epi64(a[0], b[0]), mask)
    & _mm256_testz_si256(_mm256_cmpeq_epi64(a[1], b[1]), mask)
    & _mm256_testz_si256(_mm256_cmpeq_epi64(a[2], b[2]), mask)
    & _mm256_testz_si256(_mm256_cmpeq_epi64(a[3], b[3]), mask);
}

#ifdef AVX
double bounded_jaccard_avx(const __m256i *a, const __m256i *b) {
  int num = 0;
  int denom = 0;
  for (int i=0; i<N; i++) {
    __m256i _and = _mm256_and_si256(a[i], b[i]);
    for (int j=0; j<N; j++) {
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
  for (int i=0; i<N; i++) {
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
  for (int i=0; i<N; i++) {
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
  for (int i=0; i<N; i++) {
    uint64_t ai = ((uint64_t*) a)[i];
    uint64_t bi = ((uint64_t*) b)[i];
    uint64_t _or = ai | bi;
    uint64_t _and = ai & bi;
    num += _mm_popcnt_u64(_and);
    denom += _mm_popcnt_u64(_or);
  }
  return 1. * num / denom;
}
