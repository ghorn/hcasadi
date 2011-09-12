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
  SXFunction * sxFunctionCreate(const SXMatrix * sxIn[], int numInputs, const SXMatrix * sxOut[], int numOutputs);
  void sxFunctionDelete(SXFunction * const fun);

  // getters
  int sxFunctionGetNumInputs(const FX & fun);
  int sxFunctionGetNumOutputs(const FX & fun);
  void sxFunctionGetInputsSX(const SXFunction & fun, int idx, SXMatrix & mat);
  void sxFunctionGetOutputsSX(const SXFunction & fun, int idx, SXMatrix & mat);
  int sxFunctionGetInputSize1(  int idx, const SXFunction & fun );
  int sxFunctionGetInputSize2(  int idx, const SXFunction & fun );
  int sxFunctionGetOutputSize1( int idx, const SXFunction & fun );
  int sxFunctionGetOutputSize2( int idx, const SXFunction & fun );

  // evaluate
  void sxFunctionEvaluateDMatrix(int numInputs, const DMatrix * inputs[],
				 int numOutputs, DMatrix * outputs[],
				 SXFunction & fun);
  void sxFunctionEvaluateSXMatrix(int numInputs, const SXMatrix * inputs[],
				  int numOutputs, SXMatrix * outputs[],
				  SXFunction & fun);

#ifdef __cplusplus
}
#endif


#endif //__SX_FUNCTION_INTERFACE_H__
