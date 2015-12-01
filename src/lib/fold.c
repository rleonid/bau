#include <stdio.h>

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/alloc.h>       // caml_copy_double
#include <caml/memory.h>
#include <caml/fail.h>        // caml_array_bound_error
#include <caml/callback.h>    // caml_callback
#include <caml/signals.h>
#include <caml/threads.h>

#include <caml/bigarray.h>

/** Taken from Bigarray_stub.c  TODO: give credit
 *
 */
static long caml_ba_offset(struct caml_ba_array * b, intnat * index)
{
  intnat offset;
  int i;

  offset = 0;
  if ((b->flags & CAML_BA_LAYOUT_MASK) == CAML_BA_C_LAYOUT) {
    /* C-style layout: row major, indices start at 0 */
    for (i = 0; i < b->num_dims; i++) {
      if ((uintnat) index[i] >= (uintnat) b->dim[i])
        caml_array_bound_error();
      offset = offset * b->dim[i] + index[i];
    }
  } else {
    /* Fortran-style layout: column major, indices start at 1 */
    for (i = b->num_dims - 1; i >= 0; i--) {
      if ((uintnat) (index[i] - 1) >= (uintnat) b->dim[i])
        caml_array_bound_error();
      offset = offset * b->dim[i] + (index[i] - 1);
    }
  }
  return offset;
}

CAMLprim value num_elements(value vb)
{
  CAMLparam1(vb);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  int i = 0,s=1;
  for (i = 0; i < b->num_dims; i++)
    s *= b->dim[i];

  CAMLreturn(Val_long(s));
}

CAMLprim value apply(value f, value vb, value vind)
{
  CAMLparam3(f,vb,vind);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  intnat index[CAML_BA_MAX_NUM_DIMS];
  int i;
  intnat offset;

  for (i = 0; i < b->num_dims; i++)
    index[i] = Long_val(Field(vind,i)); // TODO if |vind| < num_dims
  offset = caml_ba_offset(b, index);

  switch ((b->flags) & CAML_BA_KIND_MASK) {
    default:
      CAMLassert(0);    // TODO: fail!
      break;
    case CAML_BA_FLOAT32:
      /* Assert that unit is return ... should be guaranteed by ML. */
      caml_callback(f, caml_copy_double(((float *) b->data)[offset]));
      break;
    case CAML_BA_FLOAT64:
      caml_callback(f, caml_copy_double(((double *) b->data)[offset]));
      break;
    /* TODO:
    case CAML_BA_SINT8:
      return Val_int(((int8 *) b->data)[offset]);
    case CAML_BA_UINT8:
      return Val_int(((uint8 *) b->data)[offset]);
    case CAML_BA_SINT16:
      return Val_int(((int16 *) b->data)[offset]);
    case CAML_BA_UINT16:
      return Val_int(((uint16 *) b->data)[offset]);
    case CAML_BA_INT32:
      return caml_copy_int32(((int32_t *) b->data)[offset]);
    case CAML_BA_INT64:
      return caml_copy_int64(((int64_t *) b->data)[offset]);
    case CAML_BA_NATIVE_INT:
      return caml_copy_nativeint(((intnat *) b->data)[offset]);
    case CAML_BA_CAML_INT:
      return Val_long(((intnat *) b->data)[offset]);
    case CAML_BA_COMPLEX32:
      { float * p = ((float *) b->data) + offset * 2;
        return copy_two_doubles(p[0], p[1]); }
    case CAML_BA_COMPLEX64:
      { double * p = ((double *) b->data) + offset * 2;
        return copy_two_doubles(p[0], p[1]); }
    case CAML_BA_CHAR:
      return Val_int(((unsigned char *) b->data)[offset]);
      */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value iter(value f, value vb)
{
  CAMLparam2(f,vb);
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  int i = 0;
  int s = 1;
  for (i = 0; i < b->num_dims; i++)
    s *= b->dim[i];

  switch ((b->flags) & CAML_BA_KIND_MASK) {
    default:
      CAMLassert(0);    // TODO: fail!
      break;
    case CAML_BA_FLOAT32:
      /* Assert that unit is return or should be guaranteed by ML? */
      for(i = 0; i < s; i++)
        caml_callback(f, caml_copy_double(((float *) b->data)[i]));
      break;
    case CAML_BA_FLOAT64:
      for(i = 0; i < s; i++)
        caml_callback(f, caml_copy_double(((double *) b->data)[i]));
      break;
    /* TODO:
    case CAML_BA_SINT8:
      return Val_int(((int8 *) b->data)[offset]);
    case CAML_BA_UINT8:
      return Val_int(((uint8 *) b->data)[offset]);
    case CAML_BA_SINT16:
      return Val_int(((int16 *) b->data)[offset]);
    case CAML_BA_UINT16:
      return Val_int(((uint16 *) b->data)[offset]);
    case CAML_BA_INT32:
      return caml_copy_int32(((int32_t *) b->data)[offset]);
    case CAML_BA_INT64:
      return caml_copy_int64(((int64_t *) b->data)[offset]);
    case CAML_BA_NATIVE_INT:
      return caml_copy_nativeint(((intnat *) b->data)[offset]);
    case CAML_BA_CAML_INT:
      return Val_long(((intnat *) b->data)[offset]);
    case CAML_BA_COMPLEX32:
      { float * p = ((float *) b->data) + offset * 2;
        return copy_two_doubles(p[0], p[1]); }
    case CAML_BA_COMPLEX64:
      { double * p = ((double *) b->data) + offset * 2;
        return copy_two_doubles(p[0], p[1]); }
    case CAML_BA_CHAR:
      return Val_int(((unsigned char *) b->data)[offset]);
      */
  }

  CAMLreturn(Val_unit);
}

CAMLprim value fold(value f, value init, value vb)
{
  CAMLparam3(f,init,vb);
  CAMLlocal1 (res);
  res = init;
  struct caml_ba_array * b = Caml_ba_array_val(vb);
  int i = 0;
  int s = 1;
  for (i = 0; i < b->num_dims; i++)
    s *= b->dim[i];

  switch ((b->flags) & CAML_BA_KIND_MASK) {
    default:
      CAMLassert(0);    // TODO: fail!
      break;
    case CAML_BA_FLOAT32:
      /* Assert that unit is return or should be guaranteed by ML? */
      for(i = 0; i < s; i++)
        res = caml_callback2(f, res, caml_copy_double(((float *) b->data)[i]));
      break;
    case CAML_BA_FLOAT64:
      for(i = 0; i < s; i++)
        res = caml_callback2(f, res, caml_copy_double(((double *) b->data)[i]));
      break;
    /* TODO:
    case CAML_BA_SINT8:
      return Val_int(((int8 *) b->data)[offset]);
    case CAML_BA_UINT8:
      return Val_int(((uint8 *) b->data)[offset]);
    case CAML_BA_SINT16:
      return Val_int(((int16 *) b->data)[offset]);
    case CAML_BA_UINT16:
      return Val_int(((uint16 *) b->data)[offset]);
    case CAML_BA_INT32:
      return caml_copy_int32(((int32_t *) b->data)[offset]);
    case CAML_BA_INT64:
      return caml_copy_int64(((int64_t *) b->data)[offset]);
    case CAML_BA_NATIVE_INT:
      return caml_copy_nativeint(((intnat *) b->data)[offset]);
    case CAML_BA_CAML_INT:
      return Val_long(((intnat *) b->data)[offset]);
    case CAML_BA_COMPLEX32:
      { float * p = ((float *) b->data) + offset * 2;
        return copy_two_doubles(p[0], p[1]); }
    case CAML_BA_COMPLEX64:
      { double * p = ((double *) b->data) + offset * 2;
        return copy_two_doubles(p[0], p[1]); }
    case CAML_BA_CHAR:
      return Val_int(((unsigned char *) b->data)[offset]);
      */
  }

  CAMLreturn(res);
}

