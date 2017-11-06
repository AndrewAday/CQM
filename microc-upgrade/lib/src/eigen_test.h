#ifdef __cplusplus
#include <iostream>
#include <Eigen/Dense>
extern "C" {
#endif

typedef void * matrix_t;
matrix_t matrix_init(char, int, int);
matrix_t mmadd(matrix_t m1, matrix_t m2);
matrix_t fmadd(double s, matrix_t m);

#ifdef __cplusplus
}
#endif