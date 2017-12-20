struct structer {
  int i;
}

struct structer foo(struct structer a)
{
  print(a.i);
  a.i = a.i + 1;
  return a;
}

struct structer foo1(struct structer a)
{
  print(a.i);
  a.i = a.i + 1;
  return a;
}

int main()
{
  struct structer s;

  s = make(struct structer);
  s.i = 1;
  s => foo() => foo1();
  print(s.i);

  return 0;
}
