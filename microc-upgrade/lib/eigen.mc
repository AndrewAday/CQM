extern void print_mat(fmatrix fm);

extern float mat_index(fmatrix fm, int i, int j);
extern float mat_index_assign(fmatrix fm, int i, int j, float f);

extern int rows(fmatrix fm);
extern int cols(fmatrix fm);
extern fmatrix init_fmat_zero(int r, int c);
extern fmatrix init_fmat_const(float s, int r, int c);
extern fmatrix init_fmat_identity(int r, int c);

extern fmatrix copy(fmatrix fm);
extern void del_mat(fmatrix fm);

extern fmatrix mm_add(fmatrix fm1, fmatrix fm2);
extern fmatrix mm_sub(fmatrix fm1, fmatrix fm2);
extern fmatrix mm_mult(fmatrix fm1, fmatrix fm2);

extern fmatrix mm_div(fmatrix fm1, fmatrix fm2);
extern fmatrix dot(fmatrix fm1, fmatrix fm2);

extern fmatrix sm_add(fmatrix fm, float s);
extern fmatrix sm_sub(fmatrix fm, float s, int rev);
extern fmatrix sm_mult(fmatrix fm, float s);

// extern fmatrix sm_div(fmatrix fm, float s, int rev);
extern fmatrix sm_div(fmatrix fm, float s);

extern fmatrix smeq(fmatrix fm, float s);

extern fmatrix transpose(fmatrix fm);
extern fmatrix negate(fmatrix fm);

/*
populates each value by falling f()
Used for random initialization.
*/
fmatrix populate_fmat(fmatrix fm, fp (float) f) {
  int: i,j;
  for (i = 0; i < rows(fm); i = i + 1) {
    for (j = 0; j < cols(fm); j = j + 1) {
      fm[i,j] = f();
    }
  }
  return fm;
}

/* apply f to every element of fm */
fmatrix f_fmat(fmatrix fm, fp (float, float) f) {
  int: i,j;
  for (i = 0; i < rows(fm); i = i + 1) {
    for (j = 0; j < cols(fm); j = j + 1) {
      fm[i,j] = f(fm[i,j]);
    }
  }
  return fm;
}

int argmax(fmatrix fm) {
  int r;
  int m;
  m = 0;
  for (r = 0; r < rows(fm); r = r + 1) {
    if (fm[r,0] > fm[m,0]) {
      m = r;
    }
  }
  return m;
}

float l2_norm(fmatrix fm) {
  int r;
  float acc;
  for (r = 0; r < rows(fm); r = r + 1) {
    acc = acc + square(fm[r,0]);
  }
  return sqrt(acc);
}

float quadratic_cost(fmatrix x, fmatrix y) {
  return square(l2_norm(x - y)) * .5;
}

fmatrix quadratic_cost_prime(fmatrix x, fmatrix y) {
  return (x - y);
}
