int main(){
  fmatrix fm;
  int i;
  float[] fa;
  fa = make(float, 20);

  for (i = 0; i < 20; i = i + 1){
    fa[i] = float_of_int(i);
  }
  fm = arr_to_fmat(fa, 4, 5);
  print_mat(fm);

  fm = arr_to_fmat((float[]) {0.0, 1.0, 2.0, 3.0, 4.0, 2.3, 2.4, 2.5, 2.6}, 3, 3);
  print_mat(fm);

  return 0;
}
