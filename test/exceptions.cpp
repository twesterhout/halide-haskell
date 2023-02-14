#include <Halide.h>

int main() {
  std::cout << Halide::exceptions_enabled() << std::endl;

  try {
    Halide::Func f;
    Halide::Var i;
    f(i) = Halide::Expr{static_cast<int32_t>(2147483647)} +
           Halide::Expr{static_cast<int32_t>(10000)};
    Halide::Buffer<int32_t> b = f.realize({1});
    std::cout << "result: " << b(0) << std::endl;
  } catch (Halide::CompileError &e) {
    std::cout << "caught error: " << e.what() << std::endl;
  }

  try {
    Halide::Func f;
    Halide::Var i;
    f(i) = Halide::Expr{static_cast<int32_t>(2147483647)} +
           Halide::Expr{static_cast<int32_t>(10000)};
    Halide::Buffer<int32_t> b = f.realize({1});
    std::cout << "result: " << b(0) << std::endl;
  } catch (Halide::CompileError &e) {
    std::cout << "caught error: " << e.what() << std::endl;
  }
}