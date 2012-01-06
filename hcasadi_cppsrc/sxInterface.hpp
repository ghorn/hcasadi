// sxInterface.hpp

#ifndef __SX_INTERFACE_H__
#define __SX_INTERFACE_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    // memory management
    SX * sxCreateSymbolic(const char * const name);
    SX * sxNewDouble(double in);
    SX * sxNewInt(int in);
    void sxDelete(SX * const sx);

    // show
    void sxShow(char * stringOut, int strLen, const SX & sx);

    // math
    int sxEqual(const SX & sx0, const SX & sx1);
    void sxPlus(const SX & sx0, const SX & sx1, SX & sxOut);
    void sxMinus(const SX & sx0, const SX & sx1, SX & sxOut);
    void sxTimes(const SX & sx0, const SX & sx1, SX & sxOut);
    void sxDivide(const SX & sx0, const SX & sx1, SX & sxOut);
    void sxNegate(const SX & sxIn, SX & sxOut);
    void sxAbs(const SX & sxIn, SX & sxOut);
    int sxSignum(const SX & sxIn);
    void sxPi(SX & sxOut);
    void sxExp(const SX & sxIn, SX & sxOut);
    void sxSqrt(const SX & sxIn, SX & sxOut);
    void sxLog(const SX & sxIn, SX & sxOut);
    void sxPow(const SX & sxBase, const SX & sxExponent, SX & sxOut);
    void sxSin(const SX & sxIn, SX & sxOut);
    void sxCos(const SX & sxIn, SX & sxOut);
    void sxTan(const SX & sxIn, SX & sxOut);
    void sxArcsin(const SX & sxIn, SX & sxOut);
    void sxArccos(const SX & sxIn, SX & sxOut);
    void sxArctan(const SX & sxIn, SX & sxOut);

    // conditional
    void sxBound(const SX & lb, const SX & ub, const SX & sxIn, SX & sxOut);

#ifdef __cplusplus
}
#endif


#endif //__SX_INTERFACE_H__
