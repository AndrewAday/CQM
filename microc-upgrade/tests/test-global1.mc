int a;
int b;

void printa()
{
  print(a);
}

void incab()
{
  a = a + 1;
  b = b + 1;
}

int main()
{
  a = 42;
  b = 21;
  printa();
  incab();
  printa();
  return 0;
}
