// snoptSolverInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/matrix/matrix_tools.hpp>
#include <casadi/fx/fx.hpp>

#include "snoptSolverInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** memory management *******************/
SnoptSolver * snoptSolverCreate(const SXMatrix & inputs, const SX & objFun, const SXMatrix & constraints){
  SnoptSolver * solver = new SnoptSolver(inputs, objFun, constraints);

#ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) snoptSolverCreate {address: " << si << "}\n";
#endif

  return solver;
}


void snoptSolverDelete(SnoptSolver * const solver){
#ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) snoptSolverDelete {address: " << si << "}\n";
#endif
  delete solver;
}


double snoptSolverSolve(SnoptSolver & solver,
			const double xguess[], int nx, 
			const double xlb[], int nxlb,
			const double xub[], int nxub,
			const double flb[], int nflb,
			const double fub[], int nfub,
			double xOpt[], int nxopt)
{
  solver.setGuess(xguess, nx);
  solver.setXBounds(xlb, nxlb, xub, nxub);
  solver.setFBounds(flb, nflb, fub, nfub);

  solver.solve();

  return solver.getSolution(xOpt, nxopt);
}

