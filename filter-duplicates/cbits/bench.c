#include <unistd.h>
#include <time.h>
#include <math.h>
#include <stdio.h>

#include <chrono>
#include <functional>
#include <iostream>
#include "immintrin.h"

const int N = 1e8;

template<typename Ret, typename... Args>
Ret time_it(const std::string name, std::function<Ret(Args...)> f, Args... args) {
        struct timespec start, end;

        auto tbegin = std::chrono::high_resolution_clock::now();
        Ret r = f(args...);
        auto tend = std::chrono::high_resolution_clock::now();
        auto tduration = std::chrono::duration_cast<std::chrono::microseconds>(tend - tbegin).count();
        std::cout << name << ": " << tduration << std::endl;
        return r;
}

double bounded_jaccard(const __m256i *a, const __m256i *b);
double bounded_jaccard_avx(const __m256i *a, const __m256i *b);

double jaccard(const __m256i *a, const __m256i *b);
double jaccard_avx(const __m256i *a, const __m256i *b);

int main() {
  __m256i a[4], b[4];
  for (int i=0; i<4; i++) {
    uint64_t val[4];
    for (int j=0; j<4; j++) val[j] = rand();
    a[i] = _mm256_load_si256((__m256i*) &val);
    b[i] = _mm256_load_si256((__m256i*) &val);
  }

  auto ra1 = time_it("bounded_reg", std::function<double()>( [&a,&b]() {
        double accum = 0;
        for (int i=0; i<1000000000; i++) {
          accum += bounded_jaccard(a, b);
        }
        std::cout << accum << std::endl;
        return accum;
      }));

  auto ra2 = time_it("bounded_avx", std::function<double()>( [&a,&b]() {
        double accum = 0;
        for (int i=0; i<1000000000; i++) {
          accum += bounded_jaccard_avx(a, b);
        }
        std::cout << accum << std::endl;
        return accum;
      }));
  std::cout << (ra1 - ra2) << std::endl;

  auto rb1 = time_it("        reg", std::function<double()>( [&a,&b]() {
        double accum = 0;
        for (int i=0; i<1000000000; i++) {
          accum += jaccard(a, b);
        }
        std::cout << accum << std::endl;
        return accum;
      }));

  auto rb2 = time_it("        avx", std::function<double()>( [&a,&b]() {
        double accum = 0;
        for (int i=0; i<1000000000; i++) {
          accum += jaccard_avx(a, b);
        }
        std::cout << accum << std::endl;
        return accum;
      }));
  std::cout << (rb1 - rb2) << std::endl;

  return 0;
}
