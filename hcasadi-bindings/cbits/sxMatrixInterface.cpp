// sxMatrixInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
//#include <casadi/matrix/matrix_tools.hpp>
//#include <casadi/matrix/matrix.hpp>
#include <casadi/mx/mx.hpp>
//#include <casadi/expression_tools.hpp>

#include "sxMatrixInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** accessors *******************/
void sxMatrixAt(const SXMatrix & mat, int n, int m, SXMatrix & out){
    out = mat[n,m];
}

void sxMatrixSet(const SX & sx, int n, int m, SXMatrix & mat){
    mat[n,m] = sx;
}

int sxMatrixSize1(const SXMatrix & mat){
    return mat.size1();
}

int sxMatrixSize2(const SXMatrix & mat){
    return mat.size2();
}


/******************** math *******************/
void sxMM(const SXMatrix & m0, const SXMatrix & m1, SXMatrix & mOut){
    mOut = mul(m0, m1);
}

void sxMatrixTranspose(const SXMatrix & mIn, SXMatrix & mOut){
    mOut = SXMatrix(mIn.trans());
}

// what's going on with dimensions here?
//void sxMatrixScale(const SX & scalar, const SXMatrix & mIn, SXMatrix & mOut){
//    mOut = scalar * mIn;
//}

void sxMatrixInv(const SXMatrix & mIn, SXMatrix & mOut){
    mOut = inv(mIn);
}
