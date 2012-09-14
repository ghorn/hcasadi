#ifndef __SXM_H__
#define __SXM_H__

#include <casadi/sx/sx.hpp>
//#include <casadi/sx/sx_tools.hpp>

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
    SXMatrix * newEmpty(void);
//    SXMatrix * newZeros(int n, int m);

    // show
    void sxmShow(char * stringOut, int strLen, const SXMatrix & sxm);

    // conditional
//    void sxBound(const SXMatrix & lb, const SXMatrix & ub, const SXMatrix & sxIn, SXMatrix & sxOut);

    // dimensions
    int sxmSize1(const SXMatrix & sxm);
    int sxmSize2(const SXMatrix & sxm);

    // access
    SXMatrix * sxmAt(const SXMatrix & sxm, int n, int m);

    // concatenate
    SXMatrix * sxmVecCat(const SXMatrix * inputs[], int numInputs);
    SXMatrix * sxmVertCat(const SXMatrix * inputs[], int numInputs);
    SXMatrix * sxmHorzCat(const SXMatrix * inputs[], int numInputs);

#ifdef __cplusplus
}
#endif

#endif //__SXM_H__
