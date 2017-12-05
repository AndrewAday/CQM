extern void print_mat(fmatrix fm);

extern int rows(fmatrix fm);
extern int cols(fmatrix fm);
extern fmatrix init_fmat_zero(int r, int c);
extern fmatrix init_fmat_const(float s, int r, int c);
extern fmatrix init_fmat_identity(int r, int c);

extern fmatrix copy(fmatrix fm);

extern fmatrix mm_add(fmatrix fm1, fmatrix fm2);
extern fmatrix mm_sub(fmatrix fm1, fmatrix fm2);
extern fmatrix mm_mult(fmatrix fm1, fmatrix fm2);

extern fmatrix mm_div(fmatrix fm1, fmatrix fm2);
extern fmatrix dot(fmatrix fm1, fmatrix fm2);

extern fmatrix sm_add(fmatrix fm, float s);
extern fmatrix sm_sub(fmatrix fm, float s, int rev);
extern fmatrix sm_mult(fmatrix fm, float s);

extern fmatrix sm_div(fmatrix fm, float s, int rev);

extern fmatrix smeq(fmatrix fm, float s);

extern fmatrix transpose(fmatrix fm);
extern fmatrix negate(fmatrix fm);
