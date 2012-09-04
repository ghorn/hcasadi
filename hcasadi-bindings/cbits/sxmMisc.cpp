//#include "math.h"
//#include <cstdio>
//#include <sstream>
#include <string.h>

#include "sxmMisc.hpp"

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>


//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;


/******************** memory management *******************/
void sxmDelete(SXMatrix * const sxm){
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) deleting sx matrix at " << sxm << endl;
#endif
    delete sxm;
}


SXMatrix * createSymbolic(int n, int m, const char * const prefix){
    SXMatrix * out;
    if (n == 1 && m == 1)
        out = new SXMatrix(ssym(prefix));
    else if (m == 1)
        out = new SXMatrix(ssym(prefix, n));
    else if (n == 1)
        out = new SXMatrix(ssym(prefix, 1, m));
    else
        out = new SXMatrix(ssym(prefix, n, m));

#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) new sx matrix at " << out << ", val: " << *out << endl;
#endif
    return out;
}

// deep copy is probably needed here, depending on haskell usage
//SXMatrix * sxMatrixDuplicate(const SXMatrix & old){
//    SXMatrix * out = new SXMatrix(old);
//#ifdef COUT_MEMORY_MANAGEMENT
//    cout << "(cpp) duplicate " << out << ", val: " << *out << endl;
//#endif
//    return out;
//}


SXMatrix * newDouble(double in){
    SXMatrix * sxm = new SXMatrix(in);
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) new sx matrix double at " << sxm << ", val: " << *sxm << endl;
#endif
    return sxm;
}

SXMatrix * newInt(int in){
    SXMatrix * sxm = new SXMatrix(in);
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) new sx matrix int at " << sxm << ", val: " << *sxm << endl;
#endif
    return sxm;
}

//SXMatrix * newZeros(int n, int m){
//    SXMatrix * out = new SXMatrix( SXMatrix::zeros(n,m) );
//#ifdef COUT_MEMORY_MANAGEMENT
//    cout << "(cpp) new sx zeros at " << out << ", val: " << *out << endl;
//#endif
//    return out;
//}



/******************** show *******************/
void sxShow(char * stringOut, int strLen, const SXMatrix & sxm){
    ostringstream sxOutStream;
    sxOutStream << sxm;
    strncpy(stringOut, sxOutStream.str().c_str(), strLen-1);

    if (sxOutStream.str().length() > strLen)
        cerr << "(cpp) ERROR - sxShow trying to write " << sxOutStream.str().length() << " characters to output string with capacity of " << strLen << " characters\n";
}


///*************** bound ***************/
//void sxBound(const SXMatrix & lb, const SXMatrix & ub, const SXMatrix & sxIn, SXMatrix & sxOut){
//    sxOut = sxIn + (ub - sxIn)*(sxIn > ub) + (lb - sxIn)*(sxIn < lb);
//}
