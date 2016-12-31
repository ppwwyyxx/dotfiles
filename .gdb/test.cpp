//File: test.cpp
//Author: Yuxin Wu <ppwwyyxxc@gmail.com>

#include <iostream>
#include <cstring>
#include <cstdio>
#include <limits>
#include <vector>
#include <unordered_map>
#include <set>
#include <iterator>
#include <unordered_set>
#include <queue>
using namespace std;
#define MSET(ARR, x) memset(ARR, x, sizeof(ARR))
#define REP(x, y) for (auto x = decltype(y){0}; x < (y); x ++)
#define REPL(x, y, z) for (auto x = decltype(z){y}; x < (z); x ++)
#define REPD(x, y, z) for (auto x = decltype(z){y}; x >= (z); x --)
#define P(a) std::cout << (a) << std::endl
#define PP(a) std::cout << #a << ": " << (a) << std::endl
#define PA(arr) \
	do { \
		std::cout << #arr << ": "; \
		std::copy(begin(arr), end(arr), std::ostream_iterator<std::remove_reference<decltype(arr)>::type::value_type>(std::cout, " ")); \
		std::cout << std::endl;  \
	} while (0)
#include <unordered_set>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>

int main() {
  auto M = cv::imread("/home/wyx/cat2.jpg", cv::IMREAD_COLOR);
  int a;
  cin >> a;

}
