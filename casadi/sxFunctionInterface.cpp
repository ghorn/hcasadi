// sxFunctionInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>

#include "sxFunctionInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** memory management *******************/
SXFunction * sxFunctionCreate(const SXMatrix & sxIn, const SXMatrix & sxOut){
  SXFunction * out = new SXFunction(sxIn, sxOut);
  return out;
}

SXFunction * sxFunctionDelete(SXFunction * const fun){
  delete fun;
}
