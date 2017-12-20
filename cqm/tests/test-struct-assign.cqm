struct foo {
    float f;
    int i;
}

struct foo foo_global;

int main()
{
  float f;
  struct foo foo;

  foo_global = make(struct foo);
  foo = make(struct foo);

  foo_global.f = 2.4;
  print_float(foo_global.f);

  foo.f = 1.0;
  f = foo.f;
  print_float(f);
  foo.f = 2.0;
  print_float(foo.f);
}
