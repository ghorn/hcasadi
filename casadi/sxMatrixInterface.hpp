// sxMatrixInterface.hpp

#ifndef __SX_MATRIX_INTERFACE_H__
#define __SX_MATRIX_INTERFACE_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  // SXMatrix
  SXMatrix * sxMatrixCreateSymbolic(char * charPrefix, int n, int m);
  SXMatrix * sxVectorCreateSymbolic(char * charPrefix, int n);
  void sxMatrixDelete(SXMatrix * const sx);
  void sxMatrixShow(char * stringOut, int strLen, const SXMatrix & sx);

  void sxVectorAt(const SXMatrix & vec, int n, SX & out);
  void sxMatrixAt(const SXMatrix & mat, int n, int m, SX & out);

#ifdef __cplusplus
}
#endif


#endif //__SX_MATRIX_INTERFACE_H__
