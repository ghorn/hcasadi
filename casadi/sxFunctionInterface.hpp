// sxFunctionInterface.hpp

#ifndef __SX_FUNCTION_INTERFACE_H__
#define __SX_FUNCTION_INTERFACE_H__

#include <casadi/sx/sx.hpp>
#include <casadi/fx/sx_function.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  // memory management
  SXFunction * sxFunctionCreate(const SXMatrix & sxIn, const SXMatrix & sxOut);
  SXFunction * sxFunctionDelete(SXFunction * const fun);

#ifdef __cplusplus
}
#endif


#endif //__SX_FUNCTION_INTERFACE_H__
