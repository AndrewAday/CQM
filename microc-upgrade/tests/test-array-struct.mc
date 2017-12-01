struct foo {
  float f;
  int i;
}

void bar(struct foo foo)
{
  foo.f = 4.5;
}

int main()
{
  struct foo[] arr;
  struct foo foo;
  arr = make(struct foo, 2);
  foo = arr[0];
  bar(foo);
  foo.i = 2;
  print_float(foo.f);
  print(foo.i);
  return 0;
}
