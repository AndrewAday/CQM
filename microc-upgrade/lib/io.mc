void print(int i) { printf("%d\n", i); }
void printb(bool b) { printf("%d\n", b); }
void print_float(float f) { printf("%f\n", f); }
void print_string(string s) { printf("%s\n", s); }
void print_line() { printf("\n"); }

void print_fmat_arr(fmatrix[] arr) {
  int i;
  for (i = 0; i < len(arr); i = i + 1) {
    print_mat(arr[i]);
  }
}

void print_fmat_arr_dims(fmatrix[] arr) {
  int i;
  for (i = 0; i < len(arr); i = i + 1) {
    printf("rows: %d cols: %d\n", rows(arr[i]), cols(arr[i]));
  }
}

void print_mnist_image(fmatrix fm) {
  int: i, j, idx;
  float val;
  for (i = 0; i < 28; i = i + 1) {
    for (j = 0; j < 28; j = j + 1) {
      idx = 28*i + j;
      val = fm[idx, 0];
      if (val > 0.01) {
        printf("[0]");
      } else {
        printf("[ ]");
      }
      if (j == 27) {
        print_line();
      }
    }
  }
}
