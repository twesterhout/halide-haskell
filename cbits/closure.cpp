#include <Halide.h>
#include <HsFFI.h>
#include <callback.h>

extern "C" void destroy_closure_and_callable(callback_t closure) {
  if (!is_callback(reinterpret_cast<void *>(closure))) {
    fprintf(stderr, "[halide-haskell] destroy_closure_and_callable(): trying "
                    "to destroy a normal function pointer. This should never "
                    "happen. Please, submit a bug report.\n"
                    "Aborting...\n");
    abort();
  }

  using data_type = std::tuple<Halide::Callable, void *>;
  auto *ctx = static_cast<data_type *>(callback_data(closure));
  if (ctx == nullptr) {
    fprintf(stderr, "[halide-haskell] destroy_closure_and_callable(): "
                    "callback_data is NULL. This should never happen. Please, "
                    "submit a bug report.\n"
                    "Aborting...\n");
    abort();
  }
  auto *stable_ptr = std::get<1>(*ctx);

  free_callback(closure);
  if (stable_ptr != nullptr)
    hs_free_stable_ptr(stable_ptr);
  delete ctx;
}
