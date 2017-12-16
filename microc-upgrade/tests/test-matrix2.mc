int main(){
	fmatrix fm1;
	fmatrix fm2;
	fmatrix fm3;



	fm1 = init_fmat_zero(5, 5);
	fm2 = init_fmat_const(2.5, 5, 5);
	fm3 = init_fmat_const(1.23, 2, 8);

	print_mat((fm1 + 1.0) + fm2);
	fm1 = fm1 + 1.0;
	print_mat((fm1 + 12.0) .. fm2);  // matrix mult
	print_mat(fm1 * fm2);	 // hadamard product

	print_mat((fm3 + 3.)^);
	print_mat(fm3 + 3.);
	print_mat(fm3 / 3.);


	return 0;
}
