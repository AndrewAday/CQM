struct foo {
  int i;
  float f;
}


int main()
{
  struct foo foo;
  struct foo foo2;
  struct foo[] foo_arr;
  foo_arr = make(struct foo, 1);
  foo = make(struct foo);
  foo.i = 1;
  foo.f = 3.14;
  print(foo.i);
  print_float(foo.f);

  foo_arr[0] = foo;
  print(foo.i);
  print_float(foo.f);
  foo.i = 2;
  foo.f = 4.12;
  foo2 = foo_arr[0];
  print(foo2.i);
  print_float(foo2.f);




  return 0;
}
