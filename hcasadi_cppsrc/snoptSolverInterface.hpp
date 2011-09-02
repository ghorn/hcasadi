// snoptSolverInterface.hpp

#ifndef __SNOPT_SOLVER_INTERFACE_H__
#define __SNOPT_SOLVER_INTERFACE_H__

#include <casadi/sx/sx.hpp>
#include <casadi/fx/sx_function.hpp>

#include "SnoptSolver.hpp"

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  SnoptSolver * snoptSolverCreate(const SXMatrix & inputs, const SX & objFun, const SXMatrix & constraints);
  void snoptSolverDelete(SnoptSolver * const solver);
  double snoptSolverSolve(SnoptSolver & solver,
			  const double xguess[],
			  const double xlb[],
			  const double xub[],
			  const double flb[],
			  const double fub[],
			  double xOpt[]);

#ifdef __cplusplus
}
#endif


#endif //__SNOPT_SOLVER_INTERFACE_H__
