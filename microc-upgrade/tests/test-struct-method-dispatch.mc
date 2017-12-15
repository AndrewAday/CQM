struct foo {
  float f;
}

[struct foo s] bar() void {
  s.f = 3.14;
  return;
}

int main()
{
  struct foo f;

  f = make(struct foo);
  f.bar();
  print_float(f.f);

  return 0;
}
