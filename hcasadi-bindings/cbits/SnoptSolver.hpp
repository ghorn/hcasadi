// SnoptSolver.hpp
// Greg Horn

#pragma once

#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/fx_tools.hpp>
#include <casadi/stl_vector_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;


#ifdef __cplusplus
extern "C" {
#endif

#include <cexamples/snopt.h>
#include <cexamples/snfilewrapper.h>

#ifdef __cplusplus
}
#endif

#define FIRST_FORTRAN_INDEX 1
#define SNOPT_INFINITY 1e25

class SnoptSolver
{
public:
    ~SnoptSolver(void);
    SnoptSolver(const SXMatrix & designVariables, const SX & objFun, const SXMatrix & constraints);

    void setGuess(const DMatrix & _xGuess);
    void setXBounds(const DMatrix & _xlb, const DMatrix & _xub);
    void setFBounds(const DMatrix & _Flb, const DMatrix & _Fub);
    double getSolution(DMatrix & _xOpt);

    void solve(void);

private:
    // function for nonlinear part of ftotal
    SXFunction Fnonlinear;

    // function for jacobian of Fnonlinear
    SXFunction Gfcn;

    // workaround for constants added to nonlinear constraints
    doublereal * Foffset;

    static int userfcn( integer    *Status, integer *n,    doublereal x[],
                        integer    *needF,  integer *neF,  doublereal F[],
                        integer    *needG,  integer *neG,  doublereal G[],
                        char       *cu,     integer *lencu,
                        integer    iu[],    integer *leniu,
                        doublereal ru[],    integer *lenru );
  
    doublereal *    x; // initial design variables
    doublereal * xlow; // x lower bound
    doublereal * xupp; // x upper bound
    doublereal * xmul; // x lagrange multipliers
    integer *  xstate; // state of initial design variables

    doublereal *    F; // initial functions
    doublereal * Flow; // F lower bound
    doublereal * Fupp; // F upper bound
    doublereal * Fmul; // F lagrange multipliers
    integer *  Fstate; // state of initial functions

    integer   n; // number of design variables
    integer neF; // number of functions (objective plus constraints)

    doublereal objAdd; // constant to be added to objective function for printing
    integer    objRow; // row of F which contains objective function

    // linear part of jacobian
    integer     neA; // number of non-zero elements in A ( must be >= 0 )
    integer    lenA; // length of iAfun/jAvar/A ( must be >=1 )
    integer * iAfun; // row indeces of sparse A
    integer * jAvar; // col indeces of sparse A
    doublereal *  A; // A

    // nonlinear part of jacobian
    integer     neG; // number of non-zero elements in G ( >= 0 )
    integer    lenG; // length of iGfun/jGvar ( must be >= 1)
    integer * iGfun; // row indeces of sparse jacobian
    integer * jGvar; // col indeces of sparse jacobian
};
