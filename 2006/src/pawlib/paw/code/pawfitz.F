*
* $Id: pawfitz.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawfitz.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/13 27/02/94  15.30.44  by  Rene Brun
*-- Author :    Rene Brun   27/02/94
      SUBROUTINE PAWFITZ(N,X,Y,EY,NPAR,IFLAG,NPFITS)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        Routine called by HFITV/HFCNV when option 'Z' is given  *
C.    *        Invokes the COMIS routine at address JADF               *
C.    *                                                                *
C.    ******************************************************************
#include "paw/pawcfu.inc"
      DIMENSION X(1),Y(1),EY(1)
C.
C.    ------------------------------------------------------------------
C.
#if defined(CERNLIB_COMIS)
      CALL CSJCAL(JADF,7,N,X,Y,EY,NPAR,IFLAG,NPFITS,X,X,X)
#endif
*
      END
