/* Trig fns */
extern float sin(float x);
extern float cos(float x);
extern float tan(float x);
extern float sinh(float x);
extern float cosh(float x);
extern float tanh(float x);
extern float asin(float x);
extern float acos(float x);
extern float atan(float x);

extern float fabs(float x);
extern float exp(float x);
extern float log(float x);  // this is natural log
extern float log10(float x); // base 10 log
extern float pow(float x, float y);
extern int modulo(int x, int y);

extern int rand();
extern void srand(int seed);

float sqrt(float x) { return pow(x, 0.5); }
float square(float x) { return pow(x, 2.); }
float max(float x, float y) {
  if (x >= y) {
    return x;
  }
  return y;
}

int min_int(int x, int y) {
  if (x > y) {
    return y;
  }
  return x;
}

/*
sample from normal distribution using two uniform variables.
Code found at:  https://phoxis.org/2013/05/04/generating-random-numbers-from-normal-distribution-in-c/
*/

float rand_norm(float mu, float sigma) {
  float: U1, U2, W, mult, X1, X2;
  float RAND_MAX;

  RAND_MAX = 2147483647.0;

  U1 = -1. + (float_of_int(rand()) / RAND_MAX) * 2.;
  U2 = -1. + (float_of_int(rand()) / RAND_MAX) * 2.;
  W = pow(U1, 2.) + pow(U2, 2.);

  while (W >= 1. || W == 0.) {
    U1 = -1. + (float_of_int(rand()) / RAND_MAX) * 2.;
    U2 = -1. + (float_of_int(rand()) / RAND_MAX) * 2.;
    W = pow(U1, 2.) + pow(U2, 2.);
  }

  mult = sqrt((-2. * log(W)) / W);
  X1 = U1 * mult;
  X2 = U2 * mult;

  return (mu + sigma * X1);
}

void shuffle(int[] arr) {
  int: i, j, k, n, RAND_MAX;
  RAND_MAX = 2147483647;
  n = len(arr);
  for (i = 0; i < n - 1; i = i + 1) {
    j = i + rand() / (RAND_MAX / (n - i) + 1);
    k = arr[j];
    arr[j] = arr[i];
    arr[i] = k;
  }
}
