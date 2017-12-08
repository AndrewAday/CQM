int add(int x, int y) {
  return x + y;
}

void print_bin(fp (int, int, int) f, int x, int y) {
  print(f(x, y));
  return;
}

int main() {
  print_bin(make(add), 7, 35);

  return 0;
}
