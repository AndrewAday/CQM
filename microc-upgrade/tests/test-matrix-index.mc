int main()
{
  fmatrix fm;
  int i;

  fm = init_fmat_const(2.45, 1, 6);

  print_float(fm[1,1]);

  return 0;
}
