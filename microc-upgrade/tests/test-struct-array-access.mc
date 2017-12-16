struct foo {
  fmatrix[] fms;
  int[] a;
}

int main()
{
  struct foo foo;
  fmatrix fm;

  foo = make(struct foo);
  foo.a = make(int,5);
  foo.fms = make(fmatrix, 1);
  fm = init_fmat_identity(3,3);
  print_mat(fm);
  foo.fms[0] = init_fmat_identity(4,4);
  print_mat(foo.fms[0]);
  foo.fms[0] = fm;
  print_mat(foo.fms[0]);
  fm[0,0] = 3.14;
  print_mat(foo.fms[0]);

  return 0;
}
