#ifndef __NLP_H__
#define __NLP_H__

#include <casadi/sx/sx.hpp>
#include <casadi/fx/sx_function.hpp>
#include <interfaces/ipopt/ipopt_solver.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

    void deleteSolver(NLPSolver * solver);
    NLPSolver * createIpoptSolver( const SXMatrix & designVariables,
                                   const SXMatrix & objFun,
                                   const SXMatrix & constraints );
    void solve( NLPSolver & solver );

#ifdef __cplusplus
}
#endif

#endif //__NLP_H__
