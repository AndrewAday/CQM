float foo(float f){
  return f * 2.0 + 4.0;
}

int main(){
  fmatrix fm;
  fmatrix fm2;
  fmatrix fm3;
  fm = init_fmat_const(4.0, 3, 3);
  fm2 = map(fm, foo);
  fm3 = map(fm, sqrt);

  print_mat(fm2);
  print_mat(fm3);
  return 0;
}
