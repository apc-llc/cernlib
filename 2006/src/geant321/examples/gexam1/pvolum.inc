*
* $Id: pvolum.inc,v 1.1.1.1 1995/10/24 10:22:07 cernlib Exp $
*
* $Log: pvolum.inc,v $
* Revision 1.1.1.1  1995/10/24 10:22:07  cernlib
* Geant
*
*
*
* pvolum.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/PVOLUM/ NL,NR,IMAT,X0
#endif
*
* pvolum.inc
*
#if defined(CERNLIB_INCLUDE)
#include "incbeg.inc"
     +   pvolum
#include "incend.inc"
#endif
