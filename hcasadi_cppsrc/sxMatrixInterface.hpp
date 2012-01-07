// sxMatrixInterface.hpp

#ifndef __SX_MATRIX_INTERFACE_H__
#define __SX_MATRIX_INTERFACE_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    // getters/setters
    void sxMatrixAt(const SXMatrix & mat, int n, int m, SXMatrix & out);
    void sxMatrixSet(const SX & sx, int n, int m, SXMatrix & mat);

    // dimensions
    int sxMatrixSize1(const SXMatrix & mat);
    int sxMatrixSize2(const SXMatrix & mat);

    // math
    void sxMM(const SXMatrix & m0, const SXMatrix & m1, SXMatrix & mOut);
    void sxMatrixTranspose(const SXMatrix & mIn, SXMatrix & mOut);
//    void sxMatrixScale(const SX & scalar, const SXMatrix & mIn, SXMatrix & mOut);
    void sxMatrixInv(const SXMatrix & mIn, SXMatrix & mOut);

#ifdef __cplusplus
}
#endif


#endif //__SX_MATRIX_INTERFACE_H__
