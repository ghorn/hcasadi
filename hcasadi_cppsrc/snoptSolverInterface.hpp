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
			  const double xguess[], int nx, 
			  const double xlb[], int nxlb,
			  const double xub[], int nxub,
			  const double flb[], int nflb,
			  const double fub[], int nfub,
			  double xOpt[], int nxopt);

#ifdef __cplusplus
}
#endif


#endif //__SNOPT_SOLVER_INTERFACE_H__
