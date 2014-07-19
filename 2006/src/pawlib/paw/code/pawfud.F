*
* $Id: pawfud.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawfud.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.05/01 16/12/88  17.11.21  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      DOUBLE PRECISION FUNCTION PAWFUD(X)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        Function called by the HBOOK fitting routines           *
C.    *        Invokes the COMIS function at address JADF              *
C.    *                                                                *
C.    ******************************************************************
#include "paw/pawcfu.inc"
      DOUBLE PRECISION XX,CSDJCL
C.
C.    ------------------------------------------------------------------
C.
#if defined(CERNLIB_COMIS)
      XX=X
      PAWFUD=CSDJCL(JADF,1,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
#endif
#if !defined(CERNLIB_COMIS)
      PAWFUD=0.
#endif
*
      END
