void foo(float[] arr, float f)
{
  arr[2] = f;
}

string[] global_arr;

int main()
{
  float[] arr;
  float[] arr1;
  global_arr = make(string, 1);
  global_arr[0] = "arrays work!";
  print_string(global_arr[0]);
  arr = make(float, 4);
  arr[0] = 1.;
  print_float(arr[0]);
  foo(arr, 2.);
  print_float(arr[2]);
  arr1 = arr;
  print_float(arr1[0]);
  print_float(arr1[2]);
  return 0;
}
