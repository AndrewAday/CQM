#include "eigen_test.h"


Eigen::MatrixXd* mat_cast(matrix_t undef_mptr){
	return static_cast<Eigen::MatrixXd*>(undef_mptr);
}

matrix_t matrix_init(char t, int d1, int d2){
	Eigen::MatrixXd * tmp = new Eigen::MatrixXd(d1, d2);
	(*tmp).setZero();
	std::cout << *tmp << std::endl;
	return tmp;
}

matrix_t mmadd(matrix_t undef_mptr1, matrix_t undef_mptr2){
	Eigen::MatrixXd* def_mptr1 = mat_cast(undef_mptr1);
	Eigen::MatrixXd* def_mptr2 = mat_cast(undef_mptr2);
	*def_mptr1 += *def_mptr2;
	return def_mptr1;
}
