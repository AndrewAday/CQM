int main()
{
  fmatrix fm;

  fm = init_fmat_zero(4,5);
  print(rows(fm));
  print(cols(fm));
  fm = init_fmat_zero(3,7);
  print(rows(fm));
  print(cols(fm));

  return 0;
}
