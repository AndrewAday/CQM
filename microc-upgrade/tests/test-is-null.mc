struct foo {
  float f;
}

int main()
{
  fmatrix fm;
  struct foo foo;

  if (is_null(foo)) {
    print_string("is null");
  }

  foo = make(struct foo);

  if (is_null(fm)) {
    print_string("is null");
  }

  fm = init_fmat_const(5.,3,3);
  if (!is_null(fm)) {
    print_string("not null");
  }

  if (!is_null(foo)) {
    print_string("not null");
  }

  return 0;
}
