// sxMatrixInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>

#include "sxMatrixInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;

SXMatrix * sxMatrixCreateSymbolic(char * charPrefix, int n, int m){
  string prefix;
  prefix.assign(charPrefix);
  SXMatrix * out = new SXMatrix(create_symbolic(prefix, n, m));
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) new sx matrix at " << out << ", val: " << *out << endl;
  #endif
  return out;
}

SXMatrix * sxVectorCreateSymbolic(char * charPrefix, int n){
  string prefix;
  prefix.assign(charPrefix);
  SXMatrix * out = new SXMatrix(create_symbolic(prefix, n));
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) new sx matrix at " << out << ", val: " << *out << endl;
  #endif
  return out;
}

void sxMatrixDelete(SXMatrix * const sx){
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) deleting sx matrix at " << sx << endl;
  #endif
  delete sx;
}

void sxMatrixShow(char * stringOut, int strLen, const SXMatrix & sx){
  ostringstream sxOutStream;
  sxOutStream << sx;
  strncpy(stringOut, sxOutStream.str().c_str(), strLen);

  if (sxOutStream.str().length() > strLen)
    cerr << "(cpp) ERROR - sxMatrixShow trying to write " << sxOutStream.str().length() << " characters to output string with capacity of " << strLen << " characters\n";
}

void sxVectorAt(const SXMatrix & vec, int n, SX & out){
  out = vec.at(n);
}

void sxMatrixAt(const SXMatrix & mat, int n, int m, SX & out){
  out = SX( mat[n,m] );
}
