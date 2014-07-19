*
* $Id: pawfun.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawfun.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 11/05/93  10.07.13  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      FUNCTION PAWFUN(X)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        Function called by the HBOOK fitting routines           *
C.    *        Invokes the COMIS function at address JADF              *
C.    *                                                                *
C.    ******************************************************************
#include "paw/pawcfu.inc"
C.
C.    ------------------------------------------------------------------
C.
#if defined(CERNLIB_COMIS)
      PAWFUN=CSR1FN(JADF,X)
#endif
#if !defined(CERNLIB_COMIS)
      PAWFUN=0.
#endif
*
      END
