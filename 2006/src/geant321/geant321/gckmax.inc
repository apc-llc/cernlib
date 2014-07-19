*
* $Id: gckmax.inc,v 1.2 2002/04/11 11:17:04 mclareni Exp $
*
* $Log: gckmax.inc,v $
* Revision 1.2  2002/04/11 11:17:04  mclareni
* Increase MXGKIN to 250
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_GCKMAX_INC
#define CERNLIB_GEANT321_GCKMAX_INC
*
*
* gckmax.inc
*
#if !defined(CERNLIB_INCLUDE)
      INTEGER MXGKIN
      PARAMETER (MXGKIN=250)
#endif
*
* gckmax.inc
*
#if defined(CERNLIB_INCLUDE)
*
#include "geant321/incbeg.inc"
     +   gckmax
#include "geant321/incend.inc"
#endif

#endif
