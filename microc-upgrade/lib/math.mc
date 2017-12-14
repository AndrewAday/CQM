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
float sqrt(float x) { return pow(x, 0.5); }
