int main()
{
  float[] f_arr;
  f_arr = make(float, 0);
  print(len(f_arr));
  f_arr = append(f_arr, 1.);
  print(len(f_arr));
  print_float(f_arr[0]);
  f_arr = append(f_arr, 2.4);
  print(len(f_arr));
  print_float(f_arr[0]);
  print_float(f_arr[1]);
  f_arr = append(f_arr, f_arr[0]);
  print_float(f_arr[2]);
  return 0;
}
