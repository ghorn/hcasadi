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
  IpoptSolver * ipoptSolverCreateExactHessian(const SXMatrix & designVariables, const SX & objFun, const SXMatrix & constraints);
  void ipoptSolverDelete(IpoptSolver * const solver);
  double ipoptSolverSolve(IpoptSolver & solver,
			  const DMatrix & guess,
			  const DMatrix & lb, const DMatrix & ub,
			  const DMatrix & gMin, const DMatrix & gMax,
			  DMatrix & sol);

#ifdef __cplusplus
}
#endif


#endif //__IPOPT_SOLVER_INTERFACE_H__
