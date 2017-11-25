struct foo {
    float f;
    int i;
}

void struct_function(struct foo f, int i)
{
  f.i = i;
  return;
}

int main()
{
  float what;
  struct foo f1;
  struct_function(f1, 2);
  print(f1.i);
  struct_function(f1, 3);
  print(f1.i);
}
