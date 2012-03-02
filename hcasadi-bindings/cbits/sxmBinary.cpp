// sxmBinary.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>

#include "sxmBinary.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;

SXMatrix * sxPlus(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 + sx1);
}

SXMatrix * sxMinus(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 - sx1);
}

SXMatrix * sxTimes(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 * sx1);
}

SXMatrix * sxDivide(const SXMatrix & sx0, const SXMatrix & sx1){
    return new SXMatrix(sx0 / sx1);
}

SXMatrix * sxPow(const SXMatrix & sxBase, const SXMatrix & sxExponent){
    return new SXMatrix(pow(sxBase, sxExponent));
}
