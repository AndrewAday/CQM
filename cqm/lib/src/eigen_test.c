#include <stdio.h>
#include "eigen_test.h"

int main(){
	matrix_t tmp = init_fmat_identity(4, 4);
	matrix_t tmp2 = init_fmat_const(1, 4, 4);
	matrix_t tmp3 = init_fmat_const(2, 4, 4);

	matrix_t tmp4 = mm_add(tmp2, tmp3);
	matrix_t tmp5 = dot(tmp2, tmp3);

	printf("%f\n", mat_index(tmp3, 1,1));
	printf("%f\n", mat_index(tmp2, 1,1));

	print_mat(tmp);
	print_mat(tmp2);
	print_mat(tmp3);
	print_mat(tmp4);
	print_mat(tmp5);

	mat_index_assign(tmp, 1, 1, 3.14);
	mat_index_assign(tmp, 2, 1, 3.14);
	print_mat(tmp);

	del_mat(tmp);
	del_mat(tmp2);
	del_mat(tmp3);
	del_mat(tmp4);
	del_mat(tmp5);
}
