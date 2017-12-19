int main()
{
  fmatrix[] arr;
  fmatrix fm;

  arr = make(fmatrix, 1);
  arr[0] = init_fmat_identity(4,4);

  print_mat(arr[0]);
  fm = copy(arr[0]);

  fm[1,1] = 1.456;
  print_mat(fm);
  print_mat(arr[0]);
  free_fmat_arr(arr);

  return 0;
}
