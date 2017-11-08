#ifdef __cplusplus
#include <iostream>
#include <Eigen/Dense>
extern "C" {
#endif

typedef void * matrix_t;

void print_mat(matrix_t);

matrix_t init_fmat_zero(int, int);
matrix_t init_fmat_const(double, int, int);
matrix_t init_fmat_identity(int, int);

void del_mat(matrix_t);

matrix_t mmadd(matrix_t, matrix_t);
matrix_t mmsub(matrix_t, matrix_t);
matrix_t dot(matrix_t, matrix_t);

matrix_t fmadd(double, matrix_t);
matrix_t fmsub(double, matrix_t);
matrix_t fmprod(double, matrix_t);
matrix_t fmdiv(double, matrix_t);

matrix_t mmcoefprod(matrix_t, matrix_t);

#ifdef __cplusplus
}
#endif