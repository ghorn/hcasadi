// sxmUnary.hpp

#ifndef __SXM_UNARY_H__
#define __SXM_UNARY_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    SXMatrix * sxNegate(const SXMatrix & sxm);
    SXMatrix * sxAbs(   const SXMatrix & sxm);
    SXMatrix * sxExp(   const SXMatrix & sxm);
    SXMatrix * sxSqrt(  const SXMatrix & sxm);
    SXMatrix * sxLog(   const SXMatrix & sxm);
    SXMatrix * sxSin(   const SXMatrix & sxm);
    SXMatrix * sxCos(   const SXMatrix & sxm);
    SXMatrix * sxTan(   const SXMatrix & sxm);
    SXMatrix * sxArcsin(const SXMatrix & sxm);
    SXMatrix * sxArccos(const SXMatrix & sxm);
    SXMatrix * sxArctan(const SXMatrix & sxm);

#ifdef __cplusplus
}
#endif


#endif //__SXM_UNARY_H__
