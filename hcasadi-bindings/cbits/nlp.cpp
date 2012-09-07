#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/matrix/matrix_tools.hpp>
#include <casadi/fx/fx.hpp>

#include "nlp.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;

void deleteSolver( NLPSolver * solver ){
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) deleteSolver {address: " << solver << "}\n";
#endif
    delete solver;
}

NLPSolver * createIpoptSolver( const SXMatrix & designVariables,
                               const SXMatrix & objFun,
                               const SXMatrix & constraints ){
    // Create the NLP solver
    SXFunction ffcn(designVariables, objFun); // objective function
    SXFunction gfcn(designVariables, constraints); // constraint

    IpoptSolver * solver = new IpoptSolver(ffcn,gfcn);

    return solver;
}

void solve( NLPSolver & solver ){
    solver.solve();
}
