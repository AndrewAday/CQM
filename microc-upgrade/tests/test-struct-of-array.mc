struct foo {
  []int a;
}

int main()
{
  struct foo foo;
  []int a;
  foo = make(struct foo);
  foo.a = make(int, 5);
  a = foo.a;
  a[0] = 1;
  print(a[0]);
  print(len(a));
  return 0;
}
