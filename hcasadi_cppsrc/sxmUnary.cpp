// sxmUnary.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>

#include "sxmUnary.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;

SXMatrix * sxNegate(const SXMatrix & sxm){
    return new SXMatrix(-sxm);
}

SXMatrix * sxAbs(const SXMatrix & sxm){
    return new SXMatrix(fabs(sxm));
}

SXMatrix * sxExp(const SXMatrix & sxm){
    return new SXMatrix(exp(sxm));
}

SXMatrix * sxSqrt(const SXMatrix & sxm){
    return new SXMatrix(sqrt(sxm));
}

SXMatrix * sxLog(const SXMatrix & sxm){
    return new SXMatrix(log(sxm));
}

SXMatrix * sxSin(const SXMatrix & sxm){
    return new SXMatrix(sin(sxm));
}

SXMatrix * sxCos(const SXMatrix & sxm){
    return new SXMatrix(cos(sxm));
}

SXMatrix * sxTan(const SXMatrix & sxm){
    return new SXMatrix(tan(sxm));
}

SXMatrix * sxArcsin(const SXMatrix & sxm){
    return new SXMatrix(asin(sxm));
}

SXMatrix * sxArccos(const SXMatrix & sxm){
    return new SXMatrix(acos(sxm));
}

SXMatrix * sxArctan(const SXMatrix & sxm){
    return new SXMatrix(atan(sxm));
}
