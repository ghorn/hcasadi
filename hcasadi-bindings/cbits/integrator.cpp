#include <string.h>

#include "integrator.hpp"

#include <interfaces/sundials/cvodes_integrator.hpp>

//#define COUT_MEMORY_MANAGEMENT

using namespace CasADi::Sundials;

FX * cvodesIntegrator(FX & f){
    FX * i = new CVodesIntegrator( f );
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) creating CVODES Integrator at " << i << endl;
#endif
    return i;
}

FX * idasIntegrator(FX & f){
    FX * i = new CVodesIntegrator( f );
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) creating CVODES Integrator at " << i << endl;
#endif
    return i;
}
