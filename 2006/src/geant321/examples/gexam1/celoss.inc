*
* $Id: celoss.inc,v 1.1.1.1 1995/10/24 10:22:07 cernlib Exp $
*
* $Log: celoss.inc,v $
* Revision 1.1.1.1  1995/10/24 10:22:07  cernlib
* Geant
*
*
*
* celoss.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/CELOSS/SEL1(40),SEL1C(40),SER1(40),SER1C(40),SNPAT1(40,4),
     *              SEL2(40),SEL2C(40),SER2(40),SER2C(40),SNPAT2(40,4),
     *              EINTOT,DEDL(40),DEDR(40),FNPAT(40,4)
#endif
*
* celoss.inc
*
#if defined(CERNLIB_INCLUDE)
#include "incbeg.inc"
     +   celoss
#include "incend.inc"
#endif
