#include <talloc.h>
#include <stdio.h>

void talloc_ref_finalizer(void *ctx, void *ptr)
{
  int res = talloc_unlink(ctx, ptr);
  if (res != 0) {
    fprintf(stderr, "Failed to unlink reference of %p on %p\n", ctx, ptr);
  }
}
