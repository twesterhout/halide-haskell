#include <Halide.h>
#include <iostream>

int main() {
  Halide::Expr expr{123};
  std::ostringstream out;
  out << expr;
  std::cerr << "Output:" << out.str() << std::endl;
}
