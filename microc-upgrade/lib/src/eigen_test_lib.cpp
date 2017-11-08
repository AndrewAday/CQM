#include "eigen_test.h"


Eigen::MatrixXd* mat_cast(matrix_t undef_mptr){
	return static_cast<Eigen::MatrixXd*>(undef_mptr);
}

void print_mat(matrix_t undef_mptr){
	Eigen::MatrixXd* def_mptr = mat_cast(undef_mptr);
	std::cout << *def_mptr << std::endl;
}

matrix_t init_fmat_zero(const int d1, const int d2){
	Eigen::MatrixXd * tmp_mptr = new Eigen::MatrixXd;
	*tmp_mptr = Eigen::MatrixXd::Zero(d1, d2);
	// std::cout << *tmp_mptr << std::endl;
	return tmp_mptr;
}

matrix_t init_fmat_const(const double c, const int d1, const int d2){
	Eigen::MatrixXd* tmp_mptr = new Eigen::MatrixXd;
	*tmp_mptr = Eigen::MatrixXd::Constant(d1, d2, c);
	
 	return tmp_mptr;
}

matrix_t init_fmat_identity(const int d1, const int d2){
	Eigen::MatrixXd* tmp_mptr = new Eigen::MatrixXd;
	*tmp_mptr = Eigen::MatrixXd::Identity(d1, d2);

	return tmp_mptr;
}

void del_mat(matrix_t undef_mptr){
	Eigen::MatrixXd * def_ptr = mat_cast(undef_mptr);
	delete def_ptr;
}

matrix_t mmadd(matrix_t undef_mptr1, matrix_t undef_mptr2){
	Eigen::MatrixXd* def_mptr1 = mat_cast(undef_mptr1);
	Eigen::MatrixXd* def_mptr2 = mat_cast(undef_mptr2);
	Eigen::MatrixXd* tmp_mptr = new Eigen::MatrixXd;

	*tmp_mptr = *def_mptr1 + *def_mptr2;
	return tmp_mptr;
}

matrix_t msub(matrix_t undef_mptr1, matrix_t undef_mptr2){
	Eigen::MatrixXd* def_mptr1 = mat_cast(undef_mptr1);
	Eigen::MatrixXd* def_mptr2 = mat_cast(undef_mptr2);
	Eigen::MatrixXd* tmp_mptr = new Eigen::MatrixXd;

	*tmp_mptr = *def_mptr1 - *def_mptr2;
	return tmp_mptr;
}

matrix_t dot(matrix_t undef_mptr1, matrix_t undef_mptr2){
	Eigen::MatrixXd* def_mptr1 = mat_cast(undef_mptr1);
	Eigen::MatrixXd* def_mptr2 = mat_cast(undef_mptr2);
	Eigen::MatrixXd* tmp_mptr = new Eigen::MatrixXd;

	*tmp_mptr = *def_mptr1 * *def_mptr2;
	return tmp_mptr;
}