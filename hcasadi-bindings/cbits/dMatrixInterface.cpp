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
void dMatrixDelete(DMatrix * const d){
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) deleting d matrix at " << d << endl;
#endif
    delete d;
}

DMatrix * dMatrixZeros(int n, int m){
    DMatrix * out = new DMatrix( n, m, 0.0 );
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) new d zeros at " << out << endl;
#endif
    return out;
}


/******************** accessors *******************/
double dMatrixAt(const DMatrix & mat, int n, int m){
    return (mat[n,m]).at(0);
}

void dMatrixSetToList(int length, double * list, const DMatrix & mat){
    for (int k=0; k<length; k++)
        list[k] = (mat[k,0]).at(0);
}

void dMatrixSetFromList(int length, double * list, DMatrix & mat){
    for (int k=0; k<length; k++)
        mat[k, 0] = list[k];
}

void dMatrixSetFromLists(int rows, int cols, double * list, DMatrix & mat){
    int k=0;
    for (int row=0; row<rows; row++){
        for (int col=0; col<cols; col++){
            mat[row, col] = list[k];
            k++;
        }
    }
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

void dMatrixNegate(const DMatrix & m0, DMatrix & mOut){
    mOut = -m0;
}

void dMM(const DMatrix & m0, const DMatrix & m1, DMatrix & mOut){
    mOut = mul(m0, m1);
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

void dMatrixVertcat(const DMatrix * mIn[], int nIn, DMatrix & mOut){
    vector<DMatrix> inputs(0);
    for (int k=0; k<nIn; k++)
        inputs.push_back(*(mIn[k]));
    mOut = vertcat(inputs);
}
