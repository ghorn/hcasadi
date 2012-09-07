// sxmMath.hpp

#ifndef __SXM_MATH_H__
#define __SXM_MATH_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

//    int sxEqual(const SXMatrix & sx0, const SXMatrix & sx1);
//    int sxSignum(const SXMatrix & sxIn);

    // binary
    SXMatrix * sxmPlus(  const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxmMinus( const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxmTimes( const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxmDivide(const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxmPow(   const SXMatrix & sxBase, const SXMatrix & sxExponent);

    // unary
    SXMatrix * sxmNegate(const SXMatrix & sxm);
    SXMatrix * sxmAbs(   const SXMatrix & sxm);
    SXMatrix * sxmExp(   const SXMatrix & sxm);
    SXMatrix * sxmSqrt(  const SXMatrix & sxm);
    SXMatrix * sxmLog(   const SXMatrix & sxm);
    SXMatrix * sxmSin(   const SXMatrix & sxm);
    SXMatrix * sxmCos(   const SXMatrix & sxm);
    SXMatrix * sxmTan(   const SXMatrix & sxm);
    SXMatrix * sxmArcsin(const SXMatrix & sxm);
    SXMatrix * sxmArccos(const SXMatrix & sxm);
    SXMatrix * sxmArctan(const SXMatrix & sxm);

    // matrix
    SXMatrix * sxmMM(const SXMatrix & m0, const SXMatrix & m1);
    SXMatrix * sxmTranspose(const SXMatrix & sxm);
    SXMatrix * sxmInv(const SXMatrix & sxm);

    // differentiation
    SXMatrix * sxmGradient(const SXMatrix & expression, const SXMatrix & arguments);
    SXMatrix * sxmHessian(const SXMatrix & expression, const SXMatrix & arguments);
    SXMatrix * sxmJacobian(const SXMatrix & expression, const SXMatrix & arguments);

#ifdef __cplusplus
}
#endif


#endif //__SXM_MATH_H__
