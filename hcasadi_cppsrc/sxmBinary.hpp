// sxmBinary.hpp

#ifndef __SXM_BINARY_H__
#define __SXM_BINARY_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    SXMatrix * sxPlus(  const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxMinus( const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxTimes( const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxDivide(const SXMatrix & sx0,    const SXMatrix & sx1);
    SXMatrix * sxPow(   const SXMatrix & sxBase, const SXMatrix & sxExponent);

#ifdef __cplusplus
}
#endif


#endif //__SXM_BINARY_H__
