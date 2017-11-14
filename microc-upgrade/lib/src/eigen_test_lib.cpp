#include "eigen_test.h"

using namespace Eigen;


/* ============================ Utility Functions ============================== */

void onion_matrix_test(){
	MatrixXd tmp_m = MatrixXd::Constant(5, 5, 2.4);
	std::cout << tmp_m << std::endl;
}

MatrixXd* mat_cast(matrix_t undef_mptr){
	return static_cast<MatrixXd*>(undef_mptr);
}

void print_mat(matrix_t undef_mptr){
	MatrixXd* def_mptr = mat_cast(undef_mptr);
	std::cout << *def_mptr << std::endl;
}

/* ========================= Matrix Inititialization =========================== */

MatrixXd* init_fmat(const int d1, const int d2, const double c, const int op_id){
	MatrixXd* tmp_mptr = new MatrixXd;
	switch (op_id) {
		case 0: *tmp_mptr = MatrixXd::Zero(d1, d2); break;
		case 1: *tmp_mptr = MatrixXd::Constant(d1, d2, c); break;
		case 2: *tmp_mptr = MatrixXd::Identity(d2, d2); break;
	}

	return tmp_mptr;
}

matrix_t init_fmat_zero(const int d1, const int d2)						{return init_fmat(d1, d2, 0, 0);}
matrix_t init_fmat_const(const double c, const int d1, const int d2)	{return init_fmat(d1, d2, c, 1);}
matrix_t init_fmat_identity(const int d1, const int d2)					{return init_fmat(d1, d2, 0, 2);}

void del_mat(matrix_t undef_mptr){
	MatrixXd *  def_ptr = mat_cast(undef_mptr);
	delete def_ptr;
}


/* ========================= Matrix Operations =========================== */

MatrixXd* binary_operations(matrix_t undef_mptr1, matrix_t undef_mptr2, double scalar, int op_id){
	MatrixXd* def_mptr1 = mat_cast(undef_mptr1);
	MatrixXd* def_mptr2 = mat_cast(undef_mptr2);
	MatrixXd* tmp_mptr  = new MatrixXd;

	switch(op_id) {

		/* ======================== Matrix Matrix Operations ========================== */
		// matrix-matrix addition
		case 0: *tmp_mptr = *def_mptr1 + *def_mptr2; break;
		// matrix-matrix subtraction
		case 1: *tmp_mptr = *def_mptr1 - *def_mptr2; break;
		// matrix-matrix multiplication
		case 2: *tmp_mptr = (*def_mptr1).cwiseProduct(*def_mptr2); break;
		// matrix-matrix division
		case 3: *tmp_mptr = (*def_mptr1).cwiseQuotient(*def_mptr2); break;
		// matrix-matrix dot product
		case 4: *tmp_mptr = *def_mptr1 * *def_mptr2; break;

		/* ======================== Scalar Matrix Operations ========================= */

		case 5: *tmp_mptr = (*def_mptr1).array() + scalar; break;
		case 6: *tmp_mptr = (*def_mptr1).array() - scalar; break;
		case 7: *tmp_mptr = *def_mptr1 * scalar; break;
		case 8: *tmp_mptr = *def_mptr1 / scalar; break;
		// case 9: *tmp_mptr = (*def_mptr1).array() == scalar; break;
	}	

	return tmp_mptr;
}

MatrixXd* binary_operations(matrix_t undef_mptr1, matrix_t undef_mptr2, int op_id){
	return binary_operations(undef_mptr1, undef_mptr2, 0, op_id);
}

MatrixXd* binary_operations(matrix_t undef_mptr, double scalar, int op_id){
	MatrixXd tmp_m;
	return binary_operations(undef_mptr, &tmp_m, scalar, op_id);
}

matrix_t mmadd(matrix_t undef_mptr1, matrix_t undef_mptr2)  { return binary_operations(undef_mptr1, undef_mptr2, 0); }
matrix_t mmsub(matrix_t undef_mptr1, matrix_t undef_mptr2)  { return binary_operations(undef_mptr1, undef_mptr2, 1); }
matrix_t mmmult(matrix_t undef_mptr1, matrix_t undef_mptr2) { return binary_operations(undef_mptr1, undef_mptr2, 2); }
matrix_t mmdiv(matrix_t undef_mptr1, matrix_t undef_mptr2)  { return binary_operations(undef_mptr1, undef_mptr2, 3); }
matrix_t dot(matrix_t undef_mptr1, matrix_t undef_mptr2)	{ return binary_operations(undef_mptr1, undef_mptr2, 4); } 

matrix_t smadd(double s, matrix_t undef_mptr)	{ return binary_operations(undef_mptr, s, 5); }
matrix_t smsub(double s, matrix_t undef_mptr)	{ return binary_operations(undef_mptr, s, 6); }
matrix_t smmult(double s, matrix_t undef_mptr)	{ return binary_operations(undef_mptr, s, 7); }
matrix_t smdiv(double s, matrix_t undef_mptr)	{ return binary_operations(undef_mptr, s, 8); }
// matrix_t smeq(double s, matrix_t undef_mptr)	{ return binary_operations(undef_mptr, MatrixXd* tmp, s, 9); }

matrix_t transpose(matrix_t undef_mptr){
	MatrixXd* def_mptr = mat_cast(undef_mptr);
	MatrixXd* tmp_mptr = new MatrixXd;

	*tmp_mptr = (*def_mptr).transpose();
	return tmp_mptr;
}