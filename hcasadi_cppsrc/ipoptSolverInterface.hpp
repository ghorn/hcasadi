// ipoptSolverInterface.hpp

#ifndef __IPOPT_SOLVER_INTERFACE_H__
#define __IPOPT_SOLVER_INTERFACE_H__

#include <casadi/sx/sx.hpp>
#include <casadi/fx/sx_function.hpp>
#include <interfaces/ipopt/ipopt_solver.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  // memory management
  IpoptSolver * ipoptSolverCreate(const SXMatrix & designVariables, const SX & objFun, const SXMatrix & constraints);
  void ipoptSolverDelete(IpoptSolver * const solver);

  double ipoptSolverSolve(IpoptSolver & solver, double guess[], double lb[], double ub[], double gMin[], double gMax[], double sol[]);

#ifdef __cplusplus
}
#endif


#endif //__IPOPT_SOLVER_INTERFACE_H__
