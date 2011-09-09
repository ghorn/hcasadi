// ipoptSolverInterface.cpp

#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/matrix/matrix_tools.hpp>
#include <casadi/fx/fx.hpp>

#include "ipoptSolverInterface.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** memory management *******************/
IpoptSolver * ipoptSolverCreate(const SXMatrix & designVariables, const SX & objFun, const SXMatrix & constraints){

  vector<SXMatrix> inputs(1);
  inputs[0] = designVariables;

  // Create the NLP solver
  SXFunction ffcn(designVariables, objFun); // objective function
  SXFunction gfcn(designVariables, constraints); // constraint

  gfcn.setOption("ad_mode","reverse");

  IpoptSolver * solver = new IpoptSolver(ffcn,gfcn);
  //IpoptSolver * solver = new IpoptSolver(ffcn,gfcn,FX(),Jacobian(gfcn));
  //IpoptSolver * solver = new IpoptSolver( ffcn, gfcn, hfcn,  FX());

  // Set options
  solver->setOption("tol",1e-15);
  //solver->setOption("hessian_approximation","limited-memory");
  //solver->setOption("hessian_approximation","exact");

  // initialize the solver
  solver->init();

  return solver;
}

IpoptSolver * ipoptSolverCreateExactHessian(const SXMatrix & designVariables, const SX & objFun, const SXMatrix & constraints){

  // hessian
  SXMatrix sigma = create_symbolic("sigma", 1);
  SXMatrix lambda = create_symbolic("lambda", constraints.size1());
  SX lagrangian = sigma.at(0)*objFun;
  for (int k=0; k<constraints.size1(); k++)
    lagrangian += lambda.at(k)*constraints.at(k);

  SXMatrix h = hessian(lagrangian, designVariables);

  vector<SXMatrix> inputs(3);
  inputs[0] = designVariables;
  inputs[1] = lambda;
  inputs[2] = sigma;

  // Create the NLP solver
  SXFunction ffcn(designVariables, objFun); // objective function
  SXFunction gfcn(designVariables, constraints); // constraint
  SXFunction hfcn(inputs, h); // hessian

  gfcn.setOption("ad_mode","reverse");

  //IpoptSolver * solver = new IpoptSolver(ffcn,gfcn);
  //IpoptSolver * solver = new IpoptSolver(ffcn,gfcn,FX(),Jacobian(gfcn));
  IpoptSolver * solver = new IpoptSolver( ffcn, gfcn, hfcn,  FX());

  // Set options
  solver->setOption("tol",1e-8);
  //solver->setOption("hessian_approximation","limited-memory");
  solver->setOption("hessian_approximation","exact");

  // initialize the solver
  solver->init();

  return solver;
}


void ipoptSolverDelete(IpoptSolver * const solver){
  #ifdef COUT_MEMORY_MANAGEMENT
  cout << "(cpp) ipoptSolverDelete {address: " << solver << "}\n";
  #endif
  delete solver;
}

double ipoptSolverSolve(IpoptSolver & solver,
			const DMatrix & guess,
			const DMatrix & lb, const DMatrix & ub,
			const DMatrix & gMin, const DMatrix & gMax,
			DMatrix & sol){

  solver.setInput(    lb, NLP_LBX);
  solver.setInput(    ub, NLP_UBX);
  solver.setInput( guess, NLP_X_INIT);

  // Bounds on g
  solver.setInput( gMin, NLP_LBG);
  solver.setInput( gMax, NLP_UBG);

  // Solve the problem
  solver.solve();

  // Get output
  double cost;
  solver.getOutput(cost,NLP_COST);
  solver.getOutput(sol, NLP_X_OPT);

  return cost;
}
