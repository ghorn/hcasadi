// dMatrixInterface.hpp

#ifndef __D_MATRIX_INTERFACE_H__
#define __D_MATRIX_INTERFACE_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  // memory management
  void dMatrixDelete(DMatrix * const d);
  DMatrix * dMatrixZeros(int n, int m);

  // getters/setters
  double dMatrixAt(const DMatrix & mat, int n, int m);
  void dMatrixSet(const double scalar, int n, int m, DMatrix & mat);
  void dMatrixSetList(int length, double * list, DMatrix & mat);
  void dMatrixSetLists(int rows, int cols, double * list, DMatrix & mat);

  // dimensions
  int dMatrixSize1(const DMatrix & mat);
  int dMatrixSize2(const DMatrix & mat);

  // math
  void dMatrixPlus(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut);
  void dMatrixMinus(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut);
  void dMatrixNegate(const DMatrix & m0, DMatrix & mOut);
  void dMM(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut);
  void dMatrixTranspose(const DMatrix & mIn, DMatrix & mOut);
  int dMatrixIsEqual(const DMatrix & m0, const DMatrix & m1);
  void dMatrixScale(const double scalar, const DMatrix & mIn, DMatrix & mOut);
  void dMatrixInv(const DMatrix & mIn, DMatrix & mOut);
  void dMatrixVertcat(const DMatrix * mIn[], int nIn, DMatrix & mOut);

#ifdef __cplusplus
}
#endif


#endif //__D_MATRIX_INTERFACE_H__
