// codegen.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/matrix/matrix_tools.hpp>
#include <casadi/fx/fx.hpp>

#include "codegen.hpp"

using namespace std;
using namespace CasADi;


/*********** timing *************/
#include <casadi/fx/external_function.hpp>
#include <inttypes.h>
#include <sys/time.h>
static uint64_t
us_since_epoch()
{
    struct timeval tv;
 
    gettimeofday(&tv, NULL);  
    uint64_t micros = ((uint64_t)tv.tv_sec) * 1000000 + tv.tv_usec;

    return micros;
}


/************** codegen *************/
double generateCCode( const char * const filename, SXFunction & fun ){

    uint64_t t0 = us_since_epoch();
    fun.generateCode(filename);
    uint64_t t1 = us_since_epoch();

    return double(t1 - t0)*1e-6;
}

FX * createExternalFunction( const char * const objname ){

    FX * extFun = new ExternalFunction("./" + string(objname));
    extFun->init();

    return extFun;
}
