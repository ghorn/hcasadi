// sxMatMisc.hpp

#ifndef __SX_MAT_MISC_H__
#define __SX_MAT_MISC_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif



    // memory management
    void sxmDelete(SXMatrix * const sxm);
    SXMatrix * createSymbolic(int n, int m, const char * const prefix);
    //SXMatrix * sxMatrixDuplicate(const SXMatrix & old);
    SXMatrix * newDouble(double in);
    SXMatrix * newInt(int in);
    SXMatrix * newZeros(int n, int m);

    // show
    void show(char * stringOut, int strLen, const SXMatrix & sxm);

    // conditional
    void sxBound(const SXMatrix & lb, const SXMatrix & ub, const SXMatrix & sxIn, SXMatrix & sxOut);

#ifdef __cplusplus
}
#endif

#endif //__SX_MAT_MISC_H__
