// casadiInterface.hpp

#ifndef __CASADI_INTERFACE_H__
#define __CASADI_INTERFACE_H__

#include <casadi/sx/sx.hpp>

using namespace std;
using namespace CasADi;

#ifdef __cplusplus
extern "C"{
#endif

  SX * sxNewDouble(double in);
  SX * sxNewInt(int in);
  void sxDelete(SX * const sx);
  void sxShow(char * stringOut, int strLen, const SX & sx);
  int sxEqual(const SX & sx0, const SX & sx1);

  void sxPlus(const SX & sx0, const SX & sx1, SX & sxOut);
  void sxTimes(const SX & sx0, const SX & sx1, SX & sxOut);
  void sxMinus(const SX & sx0, const SX & sx1, SX & sxOut);

  void sxNegate(const SX & sxIn, SX & sxOut);
  void sxAbs(const SX & sxIn, SX & sxOut);
  int sxSignum(const SX & sxIn);



#ifdef __cplusplus
}
#endif


#endif //__CASADI_INTERFACE_H__
