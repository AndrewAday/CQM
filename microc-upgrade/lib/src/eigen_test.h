#ifdef __cplusplus
#include <iostream>
#include <Eigen/Dense>
extern "C" {
#endif

typedef void * matrix_t;

void print_mat(matrix_t);
void onion_matrix_test();

matrix_t init_fmat_zero(int, int);
matrix_t init_fmat_const(double, int, int);
matrix_t init_fmat_identity(int, int);

void del_mat(matrix_t);

matrix_t mmadd(matrix_t, matrix_t);
matrix_t mmsub(matrix_t, matrix_t);
matrix_t mmmult(matrix_t, matrix_t);
matrix_t mmdiv(matrix_t, matrix_t);
matrix_t dot(matrix_t, matrix_t);

matrix_t smadd(double, matrix_t);
matrix_t smsub(double, matrix_t);
matrix_t smmult(double, matrix_t);
matrix_t smdiv(double, matrix_t);
matrix_t smeq(double, matrix_t);

matrix_t transpose(matrix_t);

#ifdef __cplusplus
}
#endif