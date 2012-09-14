#include <string.h>

#include "integrator.hpp"

#include <interfaces/sundials/cvodes_integrator.hpp>
#include <interfaces/sundials/idas_integrator.hpp>

//#define COUT_MEMORY_MANAGEMENT

using namespace CasADi::Sundials;

FX * createCvodes(FX & f){
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) creating CVODES Integrator at " << i << endl;
#endif
    return new CVodesIntegrator( f );
}

FX * createIdas(FX & f){
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) creating CVODES Integrator at " << i << endl;
#endif
    return new IdasIntegrator( f );
}
