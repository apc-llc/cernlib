*
* $Id: pawfuy.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawfuy.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 11/05/93  10.07.13  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      FUNCTION PAWFUY(X,Y)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        Function called by the HBOOK fitting routines           *
C.    *        for 2-Dim histograms                                    *
C.    *        Invokes the COMIS function at address JADF              *
C.    *                                                                *
C.    ******************************************************************
#include "paw/pawcfu.inc"
C.
C.    ------------------------------------------------------------------
C.
#if defined(CERNLIB_COMIS)
      PAWFUY=CSR2FN(JADF,X,Y)
#endif
#if !defined(CERNLIB_COMIS)
      PAWFUY=0.
#endif
*
      END

