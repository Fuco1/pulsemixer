#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <pulse/introspect.h>

struct foo {
  int a[10];
};

int main(void) {
  printf("%ld\n", (long) offsetof(pa_sink_input_info, volume));
  printf("%ld\n", (long) offsetof(pa_sink_input_info, proplist));
  printf("%ld\n", (long) sizeof(struct foo));
}
