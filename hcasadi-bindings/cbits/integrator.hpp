#ifndef __INTEGRATOR_H__
#define __INTEGRATOR_H__

#include <casadi/fx/fx.hpp>

using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    FX * cvodesIntegrator( FX & f );
    FX * idasIntegrator( FX & f );

#ifdef __cplusplus
}
#endif

#endif //__INTEGRATOR_H__
