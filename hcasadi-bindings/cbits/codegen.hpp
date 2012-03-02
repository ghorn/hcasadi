// codegen.hpp

#ifndef __CODEGEN_HPP__
#define __CODEGEN_HPP__

#include <casadi/fx/sx_function.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    double generateCCode( const char * const filename, SXFunction & fun );
    FX * createExternalFunction( const char * const objname );

#ifdef __cplusplus
}
#endif


#endif //__CODEGEN_HPP__
