*
* $Id: urmips.inc,v 1.1.1.1 1995/10/24 10:22:14 cernlib Exp $
*
* $Log: urmips.inc,v $
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
* Geant
*
*
*
* urmips.inc
*
#if !defined(CERNLIB_INCLUDE)
      COMMON/URMIPS/XMIP,NTXMIP,TXMIP,TXMIP2,CALIB1,CALIB2,XKPL(60,8),
     *              XPHMIP(20,34),IDSTW
C
#endif
*
* urmips.inc
*
#if defined(CERNLIB_INCLUDE)
#include "incbeg.inc"
     +   urmips
#include "incend.inc"
#endif
