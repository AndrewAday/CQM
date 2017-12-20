struct foo {
  float f;
}

struct foo2 {
  float f;
}

[struct foo s] bar() void {
  s.f = 3.14;
  return;
}

int main()
{
  struct foo2 f;

  f = make(struct foo2);
  f.bar();

  return 0;
}
