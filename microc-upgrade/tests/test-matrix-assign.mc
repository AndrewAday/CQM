int main()
{
  fmatrix fm;
  int i;
  int j;

  fm = init_fmat_const(2.45, 3, 5);

  fm[1,1] = 1.34;
  for (i = 0; i < rows(fm); i = i + 1) {
    for (j = 0; j < cols(fm); j = j + 1) {
      print_float(fm[i,j]);
      if (j == cols(fm) - 1) {
        print_line();
      }
    }
  }

  print_mat(fm);

  return 0;
}
