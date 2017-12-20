struct inner_foo {
  float f;
}

struct foo {
  int i;
  struct inner_foo inner_foo;
}

int main()
{
  struct foo foo;
  struct inner_foo inner_foo;
  struct inner_foo inner_foo2;

  foo = make(struct foo);
  inner_foo = make(struct inner_foo);

  inner_foo.f = 3.14;
  print_float(inner_foo.f);

  foo.inner_foo = inner_foo;
  inner_foo2 = foo.inner_foo;

  print_float(inner_foo2.f);

  inner_foo2.f = 1.23;
  print_float(inner_foo.f);

  return 0;
}
