struct foo {
  fmatrix fm;
}

int main()
{
  struct foo foo;
  foo = make(struct foo);

  foo.fm = init_fmat_const(2., 3, 3);
  print_mat(foo.fm);

  return 0;
}
