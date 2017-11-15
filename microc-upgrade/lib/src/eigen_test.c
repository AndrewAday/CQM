#include "eigen_test.h"

int main(){
	matrix_t tmp = init_fmat_identity(4, 4);
	matrix_t tmp2 = init_fmat_const(1, 4, 4);
	matrix_t tmp3 = init_fmat_const(2, 4, 4);

	matrix_t tmp4 = mmadd(tmp2, tmp3);
	matrix_t tmp5 = dot(tmp2, tmp3);

	print_mat(tmp);
	print_mat(smmult(-1, tmp2));
	print_mat(tmp3);
	print_mat(tmp4);
	print_mat(tmp5);
// mmadd(tmp, tmp2);

	del_mat(tmp);
	del_mat(tmp2);
	del_mat(tmp3);
	del_mat(tmp4);
	del_mat(tmp5);


}

