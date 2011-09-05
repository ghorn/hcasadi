// dMatrixInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/matrix/matrix_tools.hpp>

#include "dMatrixInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** memory management *******************/
int dMatrixSizeOfAddress(){
  return sizeof(DMatrix*);
}

DMatrix * dMatrixDuplicate(const DMatrix & old){
  DMatrix * out = new DMatrix(old);
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) duplicate " << out << ", val: " << *out << endl;
  #endif
  return out;
}

void dMatrixDelete(DMatrix * const d){
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) deleting d matrix at " << d << endl;
  #endif
  delete d;
}

DMatrix * dMatrixZeros(int n, int m){
  DMatrix * out = new DMatrix( n, m, 0.0 );
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) new d zeros at " << out << ", val: " << *out << endl;
  #endif
  return out;
}


/******************** show *******************/
void dMatrixShow(char * stringOut, int strLen, const DMatrix & d){
  ostringstream dOutStream;
  dOutStream << d;
  strncpy(stringOut, dOutStream.str().c_str(), strLen);

  if (dOutStream.str().length() > strLen)
    cerr << "(cpp) ERROR - dMatrixShow trying to write " << dOutStream.str().length() << " characters to output string with capacity of " << strLen << " characters\n";
}


/******************** accessors *******************/
double dMatrixAt(const DMatrix & mat, int n, int m){
  return (mat.indexed(n,m)).at(0);
}

void dMatrixSet(const double d, int n, int m, DMatrix & mat){
  mat.indexed_assignment(n, m, d);
}

int dMatrixSize1(const DMatrix & mat){
  return mat.size1();
}

int dMatrixSize2(const DMatrix & mat){
  return mat.size2();
}


/******************** math *******************/
void dMatrixPlus(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut){
  mOut = m0 + m1;
}

void dMatrixMinus(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut){
  mOut = m0 - m1;
}

void dMM(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut){
  mOut = prod(m0, m1);
}

void dMatrixTranspose(const DMatrix & mIn, DMatrix & mOut){
  mOut = DMatrix(mIn.trans());
}

int dMatrixIsEqual(const DMatrix & m0, const DMatrix & m1){
  if (isEqual(m0, m1))
    return 1;
  return 0;
}

void dMatrixScale(const double scalar, const DMatrix & mIn, DMatrix & mOut){
  mOut = scalar * mIn;
}

void dMatrixInv(const DMatrix & mIn, DMatrix & mOut){
  mOut = inv(mIn);
}
