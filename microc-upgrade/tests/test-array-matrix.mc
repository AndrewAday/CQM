int main()
{
  fmatrix[] arr;
  fmatrix fm;
  arr = make(fmatrix, 1);

  arr[0] = init_fmat_const(2., 2, 3);
  print_mat(arr[0]);

  fm = init_fmat_const(3.14, 4, 5);
  print_mat(fm);

  print_string("\n");

  arr = append(arr, fm);
  print_mat(fm);
  print_string("\n");
  print_mat(arr[0]);
  print_string("\n");
  print_mat(arr[1]);
  print_line();

  fm = fm + 1.;
  print_mat(fm);
  print_line();
  print_mat(arr[1]);
  print_mat(arr[0]);

  return 0;
}
