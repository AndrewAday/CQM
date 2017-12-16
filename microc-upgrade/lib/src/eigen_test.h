#ifdef __cplusplus
#include <iostream>
#include <Eigen/Dense>
extern "C" {
#endif

typedef void * matrix_t;

/* =============== utility functions =============== */

void print_mat(matrix_t);
void onion_matrix_test();

int rows(matrix_t);
int cols(matrix_t);

matrix_t init_fmat_zero(int, int);
matrix_t init_fmat_const(double, int, int);
matrix_t init_fmat_identity(int, int);
matrix_t init_fmat_literal(double *, int, int);
matrix_t arr_to_fmat(double *, int, int);
matrix_t map(matrix_t, double (*f_ptr)(double));
matrix_t copy(matrix_t);

void del_mat(matrix_t);

/* =============== Index and Slicing =============== */

double mat_index(matrix_t, int, int);
double mat_index_assign(matrix_t, int, int, double);

/* =============== Binary Operations =============== */

// Matrix-Matrix operations
matrix_t mm_add(matrix_t, matrix_t);
matrix_t mm_sub(matrix_t, matrix_t);
matrix_t mm_mult(matrix_t, matrix_t);
matrix_t mm_div(matrix_t, matrix_t);
matrix_t dot(matrix_t, matrix_t);

// Scalar-Matrix operations
matrix_t sm_add(matrix_t, double);
matrix_t sm_sub(matrix_t, double, int);
matrix_t sm_mult(matrix_t, double);
matrix_t sm_div(matrix_t, double, int);
matrix_t smeq(matrix_t, double);

/* =============== Matrix Unary Operations =============== */
matrix_t transpose(matrix_t);
matrix_t negate(matrix_t);

#ifdef __cplusplus
}
#endif
