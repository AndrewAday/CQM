struct foo {
  int i;
}

int main()
{
  struct foo foo;
  struct foo foo2;
  struct foo[] arr;
  arr = make(struct foo, 0);
  print(len(arr));
  foo = make(struct foo);
  foo.i = 10;
  print(foo.i);
  arr = append(arr, foo);
  print(len(arr));
  print(foo.i);
  foo.i = 9;
  foo2 = arr[0];
  print(foo2.i);

  return 0;
}
