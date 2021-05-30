#include "Halide.h"

using namespace Halide;

namespace {

class foo_generator : public Generator<foo_generator> {
public:
  Input<float> alpha{"alpha"};
  Input<Buffer<float>> x{"x", 1};
  Output<Buffer<float>> y{"y", 1};

  void generate() {
    Var i{"i"};

    y(i) = alpha * x(i);

    x.dim(0).set_min(0);
    y.bound(i, 0, x.width());
    y.dim(0).set_bounds(0, x.width());
  }
};

} // namespace

HALIDE_REGISTER_GENERATOR(foo_generator, foo)
