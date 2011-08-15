// sxMatrixInterface.hpp

#ifndef __SX_MATRIX_INTERFACE_H__
#define __SX_MATRIX_INTERFACE_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  // memory management
  int sxMatrixSizeOfAddress(void);
  SXMatrix * sxMatrixCreateSymbolic(char * charPrefix, int n, int m);
  SXMatrix * sxMatrixDuplicate(const SXMatrix & old);
  void sxMatrixDuplicateAt(const SXMatrix & source, int idx, SXMatrix * destination[]);
  void sxMatrixFreeArray(SXMatrix * matArray[], int n);
  void sxMatrixDelete(SXMatrix * const sx);
  SXMatrix * sxMatrixZeros(int n, int m);

  // show
  void sxMatrixShow(char * stringOut, int strLen, const SXMatrix & sx);

  // getters/setters
  void sxMatrixAt(const SXMatrix & mat, int n, int m, SX & out);
  void sxMatrixSet(const SX & sx, int n, int m, SXMatrix & mat);

  // dimensions
  int sxMatrixSize1(const SXMatrix & mat);
  int sxMatrixSize2(const SXMatrix & mat);

  // math
  void sxMatrixPlus(const SXMatrix & m0, const SXMatrix & m1, SXMatrix & mOut);
  void sxMatrixMinus(const SXMatrix & m0, const SXMatrix & m1, SXMatrix & mOut);
  void sxMM(const SXMatrix & m0, const SXMatrix & m1, SXMatrix & mOut);
  void sxMatrixTranspose(const SXMatrix & mIn, SXMatrix & mOut);
  int sxMatrixIsEqual(const SXMatrix & m0, const SXMatrix & m1);

#ifdef __cplusplus
}
#endif


#endif //__SX_MATRIX_INTERFACE_H__
