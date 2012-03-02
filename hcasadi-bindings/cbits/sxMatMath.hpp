// sxMatMath.hpp

#ifndef __SX_MAT_MATH_H__
#define __SX_MAT_MATH_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    int sxEqual(const SXMatrix & sx0, const SXMatrix & sx1);
    int sxSignum(const SXMatrix & sxIn);

#ifdef __cplusplus
}
#endif


#endif //__SX_MAT_MATH_H__
