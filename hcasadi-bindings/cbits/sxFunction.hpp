#ifndef __SX_FUNCTION_H__
#define __SX_FUNCTION_H__

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
    int sxFunctionInputSize(  int idx, const SXFunction & fun );
    int sxFunctionInputSize1(  int idx, const SXFunction & fun );
    int sxFunctionInputSize2(  int idx, const SXFunction & fun );
    int sxFunctionOutputSize( int idx, const SXFunction & fun );
    int sxFunctionOutputSize1( int idx, const SXFunction & fun );
    int sxFunctionOutputSize2( int idx, const SXFunction & fun );

    // evaluate
    int sxFunctionEvalDouble( const int numInputs, const double * inputs[], const int inputSizes[],
                              const int numOutputs, double * outputs[], const int outputSizes[],
                              FX & fun );
    void sxFunctionEvaluateSXMatrix(int numInputs, const SXMatrix * inputs[],
                                    int numOutputs, SXMatrix * outputs[],
                                    SXFunction & fun);

    // options
    void sxFunctionSetOptionDouble(const char name[], const double val, SXFunction & fun);
    void sxFunctionSetOptionString(const char name[], const char val[], SXFunction & fun);
    void sxFunctionSetOptionInt(const char name[], const int val, SXFunction & fun);
    void sxFunctionSetOptionBool(const char name[], const int val, SXFunction & fun);

#ifdef __cplusplus
}
#endif


#endif //__SX_FUNCTION_H__
