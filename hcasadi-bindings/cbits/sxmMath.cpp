#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>

#include "sxmMath.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


//int sxEqual(const SXMatrix & sx0, const SXMatrix & sx1){
//    return isEqual(sx0, sx1);
//}
//
//int sxSignum(const SXMatrix & sxIn){
//    SXMatrix sxInSimp( sxIn );
//    simplify( sxInSimp );
//
//    SXMatrix sxFabsSimp(fabs(sxInSimp) );
//    simplify( sxFabsSimp );
//
//    if (isEqual(sxInSimp,sxFabsSimp))
//        return 1;
//    return -1;
//}

// binary
SXMatrix * sxmPlus(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 + sx1);
}

SXMatrix * sxmMinus(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 - sx1);
}

SXMatrix * sxmTimes(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 * sx1);
}

SXMatrix * sxmDivide(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 / sx1);
}

SXMatrix * sxmPow(const SXMatrix & sxBase, const SXMatrix & sxExponent){
    return new SXMatrix(pow(sxBase, sxExponent));
}

// unary
SXMatrix * sxmNegate(const SXMatrix & sxm){
    return new SXMatrix(-sxm);
}

SXMatrix * sxmAbs(const SXMatrix & sxm){
    return new SXMatrix(fabs(sxm));
}

SXMatrix * sxmExp(const SXMatrix & sxm){
    return new SXMatrix(exp(sxm));
}

SXMatrix * sxmSqrt(const SXMatrix & sxm){
    return new SXMatrix(sqrt(sxm));
}

SXMatrix * sxmLog(const SXMatrix & sxm){
    return new SXMatrix(log(sxm));
}

SXMatrix * sxmSin(const SXMatrix & sxm){
    return new SXMatrix(sin(sxm));
}

SXMatrix * sxmCos(const SXMatrix & sxm){
    return new SXMatrix(cos(sxm));
}

SXMatrix * sxmTan(const SXMatrix & sxm){
    return new SXMatrix(tan(sxm));
}

SXMatrix * sxmArcsin(const SXMatrix & sxm){
    return new SXMatrix(asin(sxm));
}

SXMatrix * sxmArccos(const SXMatrix & sxm){
    return new SXMatrix(acos(sxm));
}

SXMatrix * sxmArctan(const SXMatrix & sxm){
    return new SXMatrix(atan(sxm));
}

// matrix
SXMatrix * sxmMM(const SXMatrix & m0, const SXMatrix & m1){
    return new SXMatrix(mul(m0, m1));
}

SXMatrix * sxmTranspose(const SXMatrix & sxm){
    return new SXMatrix(sxm.trans());
}

SXMatrix * sxmInv(const SXMatrix & sxm){
    return new SXMatrix(inv(sxm));
}

// differentiation
SXMatrix * sxmGradient(const SXMatrix & expression, const SXMatrix & arguments){
    SXMatrix output = gradient(expression, arguments);
    makeDense(output);
    return new SXMatrix(output);
}

SXMatrix * sxmHessian(const SXMatrix & expression, const SXMatrix & arguments){
    SXMatrix theGradient = gradient(expression, arguments);
    makeDense( theGradient );
    SXMatrix output = jacobian( theGradient, arguments );
    makeDense( output );
    return new SXMatrix( output );
}

SXMatrix * sxmJacobian(const SXMatrix & expression, const SXMatrix & arguments){
    SXMatrix output = jacobian(expression, arguments);
    makeDense(output);
    return new SXMatrix( output );
}
