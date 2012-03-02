// sxMatMath.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>

#include "sxMatMath.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


int sxEqual(const SXMatrix & sx0, const SXMatrix & sx1){
    return isEqual(sx0, sx1);
}

int sxSignum(const SXMatrix & sxIn){
    SXMatrix sxInSimp( sxIn );
    simplify( sxInSimp );

    SXMatrix sxFabsSimp(fabs(sxInSimp) );
    simplify( sxFabsSimp );

    if (isEqual(sxInSimp,sxFabsSimp))
        return 1;
    return -1;
}
