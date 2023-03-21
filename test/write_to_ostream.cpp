#include <Halide.h>
#include <iostream>

int main() {
  Halide::Expr expr{123};
  std::cerr << expr << std::endl;
}
