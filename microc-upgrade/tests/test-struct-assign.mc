struct foo {
    float f;
    int i;
}

int main()
{
  float f;
  struct foo foo;
  foo.f = 1.0;
  f = foo.f;
  print_float(f);
  foo.f = 2.0;
  print_float(foo.f);
}
