extern void flush(); // flush stdout

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
