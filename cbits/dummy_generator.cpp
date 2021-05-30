#include "Halide.h"

using namespace Halide;

namespace {

class DummyGenerator : public Generator<DummyGenerator> {
public:
  void generate() {}
};

} // namespace

HALIDE_REGISTER_GENERATOR(DummyGenerator, dummy)
