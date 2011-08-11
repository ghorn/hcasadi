// sxInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include "sxInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;

SX * sxNewDouble(double in){
  SX * sx = new SX(in);
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) new sx at " << sx << ", val: " << *sx << endl;
  #endif
  return sx;
}

SX * sxNewInt(int in){
  SX * sx = new SX(in);
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) new sx at " << sx << ", val: " << *sx << endl;
  #endif
  return sx;
}

void sxDelete(SX * const sx){
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) deleting sx at " << sx << endl;
  #endif
  delete sx;
}

void sxShow(char * stringOut, int strLen, const SX & sx){
  ostringstream sxOutStream;
  sxOutStream << sx;
  strncpy(stringOut, sxOutStream.str().c_str(), strLen);

  if (sxOutStream.str().length() > strLen)
    cerr << "(cpp) ERROR - sxShow trying to write " << sxOutStream.str().length() << " characters to output string with capacity of " << strLen << " characters\n";
}

int sxEqual(const SX & sx0, const SX & sx1){
  return sx0.isEqual(sx1);
}

void sxPlus(const SX & sx0, const SX & sx1, SX & sxOut){
  sxOut = sx0 + sx1;
}

void sxMinus(const SX & sx0, const SX & sx1, SX & sxOut){
  sxOut = sx0 - sx1;
}

void sxTimes(const SX & sx0, const SX & sx1, SX & sxOut){
  sxOut = sx0 * sx1;
}

void sxDivide(const SX & sx0, const SX & sx1, SX & sxOut){
  sxOut = sx0 / sx1;
}

void sxNegate(const SX & sxIn, SX & sxOut){
  sxOut = -sxIn;
}

void sxAbs(const SX & sxIn, SX & sxOut){
  sxOut = fabs(sxIn);
}

int sxSignum(const SX & sxIn){
  SX sxInSimp( sxIn );
  simplify( sxInSimp );

  SX sxFabsSimp(fabs(sxInSimp) );
  simplify( sxFabsSimp );

  if ( sxInSimp.isEqual(sxFabsSimp) )
    return 1;
  return -1;
}


void sxPi(SX & sxOut){
  sxOut = SX(M_PI);
}

void sxExp(const SX & sxIn, SX & sxOut){
  sxOut = exp(sxIn);
}

void sxSqrt(const SX & sxIn, SX & sxOut){
  sxOut = sqrt(sxIn);
}

void sxLog(const SX & sxIn, SX & sxOut){
  sxOut = log(sxIn);
}

void sxPow(const SX & sxBase, const SX & sxExponent, SX & sxOut){
  sxOut = pow(sxBase, sxExponent);
}

void sxSin(const SX & sxIn, SX & sxOut){
  sxOut = sin(sxIn);
}

void sxCos(const SX & sxIn, SX & sxOut){
  sxOut = cos(sxIn);
}

void sxTan(const SX & sxIn, SX & sxOut){
  sxOut = tan(sxIn);
}

void sxArcsin(const SX & sxIn, SX & sxOut){
  sxOut = asin(sxIn);
}

void sxArccos(const SX & sxIn, SX & sxOut){
  sxOut = acos(sxIn);
}

void sxArctan(const SX & sxIn, SX & sxOut){
  sxOut = atan(sxIn);
}

