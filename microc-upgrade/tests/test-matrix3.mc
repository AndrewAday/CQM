int main()
{
  fmatrix fm1;
  fmatrix fm2;

  fm1 = init_fmat_const(2., 2, 2);
  fm2 = init_fmat_const(1.5, 2, 2);

  print_mat(fm1 * fm2);

  return 0;
}
