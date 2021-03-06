// SnoptSolver.cpp
// Greg Horn

#include <stdio.h>
#include <string.h>
#include <iostream>

#include <cstdlib>

#include "SnoptSolver.hpp"

static SnoptSolver * si;

using namespace std;
using namespace CasADi;

SnoptSolver::~SnoptSolver()
{
    delete []iGfun;
    delete []jGvar;

    delete []iAfun;
    delete []jAvar;
    delete []A;

    delete []x;
    delete []xlow;
    delete []xupp;
    delete []xmul;
    delete []xstate;

    delete []F;
    delete []Flow;
    delete []Fupp;
    delete []Fmul;
    delete []Fstate;
    delete []Foffset;
}

void
SnoptSolver::setGuess(const DMatrix & _xGuess){
    for (int k=0; k<n; k++)
        x[k] = _xGuess[k,0].at(0);
}

void
SnoptSolver::setXBounds(const DMatrix & _xlb, const DMatrix & _xub){
    for (int k=0; k<n; k++){
        xlow[k] = _xlb[k,0].at(0);
        xupp[k] = _xub[k,0].at(0);
    }
}

void
SnoptSolver::setFBounds(const DMatrix & _Flb, const DMatrix & _Fub){
    // set bound on objective function
    Flow[0] = -SNOPT_INFINITY;
    Fupp[0] = SNOPT_INFINITY;

    // set bound on nonlinear constraints
    for (int k=0; k<neF-1; k++){
        Flow[k+1] = _Flb[k,0].at(0);
        Fupp[k+1] = _Fub[k,0].at(0);
    }

    // correct for constant offset in F
    for (int k=0; k<neF; k++){
        Flow[k] -= Foffset[k];
        Fupp[k] -= Foffset[k];
    }

    // cout << endl;
    // for (int k=0; k<neF; k++)
    //   cout << "Flow[" << k << "]: " << Flow[k] << ", Fupp[" << k << "]: " << Fupp[k] << endl;
}

double
SnoptSolver::getSolution(DMatrix & _xOpt){
    for (int k=0; k<n; k++)
        _xOpt[k, 0] = x[k];
    return F[objRow - FIRST_FORTRAN_INDEX] + objAdd;
}


SnoptSolver::SnoptSolver(const SXMatrix & designVariables, const SXMatrix & objFun, const SXMatrix & constraints)
{
    // make sure mempy is ok for copying doublereal to double
    if (sizeof(double) != sizeof(doublereal)){
        cout << "\n\n----------------------------------------------------\n";
        cout << "(cpp), sizeof(doublereal) != sizeof(double)\n";
        cout << "----------------------------------------------------\n\n";
        throw 1;
    }

    si = this;
    SXMatrix ftotal = vertcat( SXMatrix(objFun), constraints );

    SXFunction Ftotal(designVariables, ftotal);
    Ftotal.init();

    /************ design variables ************/
    n = Ftotal.input().size();
    x = new doublereal[n];
    xlow = new doublereal[n];
    xupp = new doublereal[n];
    xmul = new doublereal[n];
    xstate = new integer[n];
    for (int k=0; k<n; k++){
        x[k] = 0;
        xlow[k] = -SNOPT_INFINITY;
        xupp[k] = SNOPT_INFINITY;
        xmul[k] = 0;
        xstate[k] = 0;
    }

    /*********** objective/constraint functions ***********/
    neF = Ftotal.output().size();
    objRow = FIRST_FORTRAN_INDEX;
    F = new doublereal[neF];
    Flow = new doublereal[neF];
    Fupp = new doublereal[neF];
    Fmul = new doublereal[neF];
    Fstate = new integer[neF];
    Foffset = new doublereal[neF];
    for (int k=0; k<neF; k++){
        F[k] = 0;
        Flow[k] = -SNOPT_INFINITY;
        Fupp[k] = 0;
        Fmul[k] = 0;
        Fstate[k] = 0;
        Foffset[k] = 0;
    }
    Fupp[ objRow - FIRST_FORTRAN_INDEX ] = SNOPT_INFINITY;

    /****************** jacobian *********************/
    SXMatrix fnonlinear = ftotal;

    SXFunction gradF(Ftotal.jacobian());

    vector<int> rowind,col;
    gradF.output().sparsity().getSparsityCRS(rowind,col);

    // split jacobian into constant and nonconstant elements (linear and nonlinear parts of F)
    vector<doublereal> A_;
    vector<integer> iAfun_;
    vector<integer> jAvar_;

    vector<SX> G_;
    vector<integer> iGfun_;
    vector<integer> jGvar_;

    for(int r=0; r<rowind.size()-1; ++r)
        for(int el=rowind[r]; el<rowind[r+1]; ++el){
            SXMatrix jacobElem = gradF.outputSX().getElement(r, col[el]);
            jacobElem = evaluateConstants(jacobElem);
            if (jacobElem.at(0).isConstant()){
                A_.push_back( jacobElem.at(0).getValue() );
                iAfun_.push_back( r + FIRST_FORTRAN_INDEX );
                jAvar_.push_back( col[el] + FIRST_FORTRAN_INDEX );

                // subtract out linear part
                SXMatrix linearpart = jacobElem.at(0).getValue()*designVariables[col[el]];
                fnonlinear[r] -= linearpart.at(0);
                simplify(fnonlinear.at(r));
            } else {
                G_.push_back( gradF.outputSX().getElement(r, col[el]) );
                iGfun_.push_back( r + FIRST_FORTRAN_INDEX );
                jGvar_.push_back( col[el] + FIRST_FORTRAN_INDEX );
            }
        }

    // remove constants from fnonlinear
    fnonlinear = evaluateConstants(fnonlinear);
    for (int k=0; k<neF; k++){
        if (fnonlinear.at(k).isConstant()){
            Foffset[k] = fnonlinear.at(k).getValue();
            fnonlinear[k] -= fnonlinear.at(k);
            simplify(fnonlinear.at(k));
        }
    }
    objAdd = Foffset[objRow - FIRST_FORTRAN_INDEX];

    // nonlinear function
    Fnonlinear = SXFunction( designVariables, fnonlinear );
    Fnonlinear.init();

    // linear part
    neA = A_.size();
    lenA = neA;
    if (lenA == 0) lenA = 1;

    A = new doublereal[lenA];
    iAfun = new integer[lenA];
    jAvar = new integer[lenA];

    if (neA > 0){
        copy( A_.begin(), A_.end(), A);
        copy( iAfun_.begin(), iAfun_.end(), iAfun);
        copy( jAvar_.begin(), jAvar_.end(), jAvar);
    }
        
    // nonlinear part
    neG = G_.size();
    lenG = neG;
    if (lenG == 0) lenG = 1;

    iGfun = new integer[lenG];
    jGvar = new integer[lenG];

    if (neG > 0){
        copy( iGfun_.begin(), iGfun_.end(), iGfun);
        copy( jGvar_.begin(), jGvar_.end(), jGvar);
    }

    Gfcn = SXFunction( designVariables, G_ );
    Gfcn.init();

    // for (int k=0; k<neA; k++){
    //   cout << "A[" << iAfun[k] << "," << jAvar[k] << "]: " << A[k] << endl;
    // }
    // cout << "\n";

    // for (int k=0; k<neG; k++){
    //      cout << "G[" << iGfun[k] << "," << jGvar[k] << "]: " << Gfcn.outputSX().at(k) << endl;
    // }
    // cout << "\n";

    // cout << "Fnonlinear:\n";
    // for (int k=0; k<neF; k++)
    //      cout << "F[" << k << "]: " << Fnonlinear.outputSX().at(k) << endl;
}


void
SnoptSolver::solve()
{
//  #define LENRW 20000
//  #define LENIW 10000
#define LENCW 500

#define LENRW 600000
#define LENIW 150000
    //#define LENCW 5000

    integer    minrw, miniw, mincw;
    integer    lenrw = LENRW, leniw = LENIW, lencw = LENCW;
    doublereal rw[LENRW];
    integer    iw[LENIW];
    char       cw[8*LENCW];

    integer    Cold = 0, Basis = 1, Warm = 2;

    integer    INFO;

    integer    nxname = 1, nFname = 1, npname;
    char       xnames[1*8], Fnames[1*8];
    char       Prob[200];

    integer    iSpecs = 4,  spec_len;
    integer    iSumm  = 6;
    integer    iPrint = 9,  prnt_len;

    char       printname[200];
    char       specname[200];

    integer    nS, nInf;
    doublereal sInf;
    integer    DerOpt, Major, strOpt_len;
    char       strOpt[200];

    /* open output files using snfilewrappers.[ch] */
    sprintf(specname ,   "%s", "sntoya.spc");   spec_len = strlen(specname);
    sprintf(printname,   "%s", "sntoya.out");   prnt_len = strlen(printname);

    /* Open the print file, fortran style */
    snopenappend_
        ( &iPrint, printname,   &INFO, prnt_len );

    /*     ================================================================== */
    /*     First,  sninit_ MUST be called to initialize optional parameters   */
    /*     to their default values.                                           */
    /*     ================================================================== */

    sninit_
        ( &iPrint, &iSumm, cw, &lencw, iw, &leniw, rw, &lenrw, 8*500 );

    strcpy(Prob,"snopta");
    npname = strlen(Prob);
    INFO = 0;

    /* Read in specs file (optional) */
    /* snfilewrapper_ will open the specs file, fortran style, */
    /* then call snspec_ to read in specs.                        */

    // snfilewrapper_
    //      ( specname, &iSpecs, &INFO, cw, &lencw,
    //        iw, &leniw, rw, &lenrw, spec_len, 8*lencw);

    // if( INFO != 101 )
    // {
    //      printf("Warning: trouble reading specs file %s \n", specname);
    // }


    // sprintf(strOpt,"%s","Solution yes");
    // strOpt_len = strlen(strOpt);
    // snset_
    //      ( strOpt, &iPrint, &iSumm, &INFO,
    //        cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );

    /*     ------------------------------------------------------------------ */
    /*     Tell SnoptA that userfg computes derivatives.                      */
    /*     ------------------------------------------------------------------ */

    DerOpt = 1;
    sprintf(strOpt,"%s","Derivative option");
    strOpt_len = strlen(strOpt);
    snseti_
        ( strOpt, &DerOpt, &iPrint, &iSumm, &INFO,
          cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );

    Major = 250;
    //Major = 2500;
    strcpy( strOpt,"Major Iterations limit");
    strOpt_len = strlen(strOpt);
    snseti_
        ( strOpt, &Major, &iPrint, &iSumm, &INFO,
          cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );


    integer Minor = 1000;
    strcpy( strOpt,"Minor Iterations limit");
    strOpt_len = strlen(strOpt);
    snseti_
        ( strOpt, &Minor, &iPrint, &iSumm, &INFO,
          cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );

    //integer Niter = 100000;
    integer Niter = 10000;
    strcpy( strOpt,"Iterations limit");
    strOpt_len = strlen(strOpt);
    snseti_
        ( strOpt, &Niter, &iPrint, &iSumm, &INFO,
          cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );

    doublereal major_opt_tol = 1e-2;
    strcpy(strOpt,"Major optimality tolerance");
    strOpt_len = strlen(strOpt);
    snsetr_
        ( strOpt, &major_opt_tol, &iPrint, &iSumm, &INFO,
          cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );
                     
    // integer verifyLevel = 3;
    // strcpy( strOpt,"Verify level");
    // strOpt_len = strlen(strOpt);
    // snseti_
    //      ( strOpt, &verifyLevel, &iPrint, &iSumm, &INFO,
    //        cw, &lencw, iw, &leniw, rw, &lenrw, strOpt_len, 8*500 );

    /*     ------------------------------------------------------------------ */
    /*     Solve the problem                                                  */
    /*     ------------------------------------------------------------------ */
    snopta_
        ( &Cold, &neF, &n, &nxname, &nFname,
          &objAdd, &objRow, Prob, (U_fp)userfcn,
          iAfun, jAvar, &lenA, &neA, A,
          iGfun, jGvar, &lenG, &neG,
          xlow, xupp, xnames, Flow, Fupp, Fnames,
          x, xstate, xmul, F, Fstate, Fmul,
          &INFO, &mincw, &miniw, &minrw,
          &nS, &nInf, &sInf,
          cw, &lencw, iw, &leniw, rw, &lenrw,
          cw, &lencw, iw, &leniw, rw, &lenrw,
          npname, 8*nxname, 8*nFname,
          8*500, 8*500);

    // extern int snopta_
    // ( integer *start, integer *nef, integer *n, integer *nxname, integer *nfname,
    //   doublereal *objadd, integer *objrow, char *prob, U_fp usrfun,
    //   integer *iafun, integer *javar, integer *lena, integer *nea, doublereal *a,
    //   integer *igfun, integer *jgvar, integer *leng, integer *neg,
    //   doublereal *xlow, doublereal *xupp, char *xnames, doublereal *flow, doublereal *fupp, char *fnames,
    //   doublereal *x, integer *xstate, doublereal *xmul, doublereal *f, integer *fstate, doublereal *fmul,
    //   integer *inform, integer *mincw, integer *miniw, integer *minrw,
    //   integer *ns, integer *ninf, doublereal *sinf,
    //   char *cu, integer *lencu, integer *iu, integer *leniu, doublereal *ru, integer *lenru,
    //   char *cw, integer *lencw, integer *iw, integer *leniw, doublereal *rw, integer *lenrw,
    //   ftnlen prob_len, ftnlen xnames_len, ftnlen fnames_len,
    //   ftnlen cu_len, ftnlen cw_len );

    snclose_( &iPrint );
//  snclose_( &iSpecs );
}


int SnoptSolver::userfcn
( integer    *Status, integer *n,    doublereal x[],
  integer    *needF,  integer *neF,  doublereal F[],
  integer    *needG,  integer *neG,  doublereal G[],
  char       *cu,     integer *lencu,
  integer    iu[],    integer *leniu,
  doublereal ru[],    integer *lenru )
{
    if( *needF > 0 ) {
        si->Fnonlinear.setInput(x);
        si->Fnonlinear.evaluate();
        si->Fnonlinear.getOutput(F);

        // cout << endl;
        // for (int k=0; k<*neF; k++)
        // 	 cout << "F[" << k << "]: " << F[k] << endl;
    }

    if( *needG > 0 ){
        si->Gfcn.setInput(x);
        si->Gfcn.evaluate();
        si->Gfcn.getOutput(G);

        // cout << endl;
        // for (int k=0; k<*neG; k++)
        // 	 cout << "G[" << k << "]: " << G[k] << endl;
    }

    return 0;
}
