extern fmatrix init_fmat_zero(int x, int y);
extern fmatrix init_fmat_const(float d, int x, int y);
extern fmatrix dot(fmatrix fm1, fmatrix fm2);
extern fmatrix mmadd(fmatrix fm1, fmatrix fm2);
extern fmatrix smadd(float s, fmatrix fm);
extern void print_mat(fmatrix fm);

int main(){
	fmatrix fm1;
	fmatrix fm2;

	fm1 = init_fmat_zero(5, 5);
	fm2 = init_fmat_const(2.5, 5, 5);
	
	print_mat((fm1 + 1.0) + fm2);
	fm1 = fm1 + 1.0;
	print_mat((fm1 + 12.0) .. fm2);
	print_mat(fm1 * fm2);


	return 1;
}