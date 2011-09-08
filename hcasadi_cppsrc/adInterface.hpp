// adInterface.hpp

#ifndef __AD_INTERFACE_H__
#define __AD_INTERFACE_H__

#include <casadi/sx/sx.hpp>
#include <casadi/fx/sx_function.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  // SXMatrix
  void myGradient(const SX & expression, const SXMatrix & arguments, SXMatrix & output);
  void myHessian(const SX & expression, const SXMatrix & arguments, SXMatrix & output);
  void myJacobian(const SXMatrix & expression, const SXMatrix & arguments, SXMatrix & output);

  // SXFunction
  void sxFunctionGradient(SXFunction & fun, int idx, SXMatrix & output);
  void sxFunctionJacobian(SXFunction & fun, int idxIn, int idxOut, SXMatrix & output);
  void sxFunctionHessian(SXFunction & fun, int idxIn, int idxOut, SXMatrix & output);

#ifdef __cplusplus
}
#endif


#endif //__AD_INTERFACE_H__
