// adInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/matrix/matrix_tools.hpp>

#include "adInterface.hpp"

using namespace std;
using namespace CasADi;

/************************ SXMatrix *****************************/
void myGradient(const SX & expression, const SXMatrix & arguments, SXMatrix & output){
  output = gradient(expression, arguments);
  makeDense(output);
}

void myHessian(const SX & expression, const SXMatrix & arguments, SXMatrix & output){
  SXMatrix theGradient = gradient(expression, arguments);
  makeDense( theGradient );
  output = jacobian( theGradient, arguments );
  makeDense( output );
}

void myJacobian(const SXMatrix & expression, const SXMatrix & arguments, SXMatrix & output){
  output = jacobian(expression, arguments);
  makeDense(output);
}


/************************ SXFunction *************************/
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
