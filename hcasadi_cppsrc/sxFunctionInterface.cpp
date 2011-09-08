// sxFunctionInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/matrix/matrix_tools.hpp>
#include <casadi/fx/fx.hpp>

#include "sxFunctionInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** memory management *******************/
void sxFunctionDelete(SXFunction * const fun){
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) sxFunctionDelete {address: " << fun << "}\n";
  #endif
  delete fun;
}

SXFunction * sxFunctionCreate(const SXMatrix * sxIn[], int numInputs, const SXMatrix * sxOut[], int numOutputs){
  vector<SXMatrix> inputs(numInputs);
  vector<SXMatrix> outputs(numOutputs);

  for (int k=0; k<numInputs; k++)
    inputs.at(k) = *(sxIn[k]);

  for (int k=0; k<numOutputs; k++)
    outputs.at(k) = *(sxOut[k]);

  SXFunction * fun = new SXFunction(inputs, outputs);
  fun->init();

  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) sxFunctionCreate {address: " << fun << "}\n";
  #endif

  return fun;
}


/*************** getters *****************/
int sxFunctionGetNumInputs(const FX & fun){
  return fun.getNumInputs();
}

int sxFunctionGetNumOutputs(const FX & fun){
  return fun.getNumOutputs();
}

void sxFunctionGetInputsSX(const SXFunction & fun, int idx, SXMatrix & mat){
  mat = fun.inputSX(idx);
}

void sxFunctionGetOutputsSX(const SXFunction & fun, int idx, SXMatrix & mat){
  mat = fun.outputSX(idx);
}

int sxFunctionGetInputSize1( int idx, const SXFunction & fun ){
  return fun.inputSX(idx).size1();
}

int sxFunctionGetInputSize2( int idx, const SXFunction & fun ){
  return fun.inputSX(idx).size2();
}

int sxFunctionGetOutputSize1( int idx, const SXFunction & fun ){
  return fun.outputSX(idx).size1();
}

int sxFunctionGetOutputSize2( int idx, const SXFunction & fun ){
  return fun.outputSX(idx).size2();
}


/************ evaluate *************/
void sxFunctionEvaluate(int numInputs, const DMatrix * inputs[],
			int numOutputs, DMatrix * outputs[],
			FX & fun){
  for (int k=0; k<numInputs; k++)
    fun.setInput( *(inputs[k]), k );
  fun.evaluate();
  for (int k=0; k<numOutputs; k++)
    fun.getOutput( *(outputs[k]), k );
}
