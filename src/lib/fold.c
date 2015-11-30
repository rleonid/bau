

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim value num_elements(value vb)
{
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  int i = 0,s=1;
  for (i = 0; i < b->num_dims; i++)
    s *= b->dim[i];
  return Val_long(s);
}
