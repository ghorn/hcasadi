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
SXFunction * sxFunctionCreate(const SXMatrix & sxIn, const SXMatrix & sxOut){
  SXFunction * fun = new SXFunction(sxIn, sxOut);
  fun->init();

  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) sxFunctionCreate {address: " << fun << "}\n";
  #endif

  return fun;
}

void sxFunctionDelete(SXFunction * const fun){
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) sxFunctionDelete {address: " << fun << "}\n";
  #endif
  delete fun;
}

SXFunction * sxFunctionCreateMulti(const SXMatrix * sxIn[], int numInputs, const SXMatrix * sxOut[], int numOutputs){
  vector<SXMatrix> inputs(numInputs);
  vector<SXMatrix> outputs(numOutputs);

  for (int k=0; k<numInputs; k++)
    inputs.at(k) = *(sxIn[k]);

  for (int k=0; k<numOutputs; k++)
    outputs.at(k) = *(sxOut[k]);

  SXFunction * fun = new SXFunction(inputs, outputs);
  fun->init();

  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) sxFunctionCreateMulti {address: " << fun << "}\n";
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
void sxFunctionEvaluateInputOld(FX & fun, const double inputsArray[], const int inputRows[], const int inputCols[]){
  // setup inputs
  int inputCounter = 0;
  for (int kth_input=0; kth_input<fun.getNumInputs(); kth_input++){
    DMatrix anInput(inputRows[kth_input], inputCols[kth_input], 0.0);
    for (int row=0; row<inputRows[kth_input]; row++)
      for (int col=0; col<inputCols[kth_input]; col++){
	anInput.indexed_assignment( row, col, inputsArray[inputCounter] );
	inputCounter++;
      }
    fun.setInput( anInput, kth_input );
  }

  // evaluate
  fun.evaluate();
}

void sxFunctionGetEvaluatedOutputOld(FX & fun, int outputIdx, int rows, int cols, double output[]){
  // retrieve an output
  DMatrix anOutput(rows, cols, 0.0);
  fun.getOutput( anOutput, outputIdx );

  int outputCounter = 0;
  for (int r=0; r<rows; r++){
    for (int c=0; c<cols; c++){
      output[outputCounter] = anOutput.getElement(r, c);
      outputCounter++;
    }
  }
}


void sxFunctionSetInput(int idx, FX & fun, const DMatrix & mIn){
  //  cerr << "(cpp) sxFunctionSetInput idx: " << idx << ", &fun: " << &fun << ", &mIn: " << &mIn << ", val: " << mIn << endl;
  if (idx >= fun.getNumInputs()){
    cerr << "index " << idx << " >= fun.getNumInputs() (" << fun.getNumInputs() << ")\n";
    throw 1;
  }
  fun.setInput( mIn, idx );
}


void sxFunctionEvaluate(FX & fun){
  fun.evaluate();
}

void sxFunctionGetEvaluatedOutput(int idx, SXFunction & fun, DMatrix & mOut){
  //cerr << "(cpp) sxFunctionGetEvaluatedOutput idx: " << idx << ", &fun: " << &fun << ", &mOut: " << &mOut << endl;
  if (idx >= fun.getNumOutputs()){
    cerr << "index " << idx << " >= fun.getNumOutputs() (" << fun.getNumOutputs() << ")\n";
    throw 1;
  }
  fun.getOutput( mOut, idx );
}


/*********************** differentiation *************************/
void sxFunctionGradient(SXFunction & fun, int idx, SXMatrix & output){
  output = SXMatrix( fun.grad( idx, 0 ) );
  makeDense(output);
}

void sxFunctionJacobian(SXFunction & fun, int idxIn, int idxOut, SXMatrix & output){
  output = SXMatrix( fun.jac( idxIn, idxOut ) );
  makeDense(output);
}

void sxFunctionHessian(SXFunction & fun, int idxIn, int idxOut, SXMatrix & output){
  SXMatrix theGrad(fun.grad(idxIn));
  makeDense(theGrad);
  output = jacobian(theGrad, fun.inputSX(idxOut));
  makeDense(output);
}
