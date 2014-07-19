*
* $Id: invert.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: invert.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE INVERT
C.----------------------------------------------------------------------
C.
C.    ROUTINE TO INVERTS CUMULATIVE DISTRIBUTIONS, SHOULD BE CALLED
C.    EACH TIME AN INITIALIZATION TAKES PLACE.
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION WGLU,WLQU,WHVY,YGLU,YLQU,YHVY,XLOW,XHIGH
#endif
#include "eurodec/glufix.inc"
#include "eurodec/quafix.inc"
#include "eurodec/epsfrg.inc"
      EXTERNAL WGLU,WLQU,WHVY
C--
C--   SET THE LIMITS ON THE X RANGE...
      XLOW =0.001
      XHIGH=0.999
C--
C--   PREPARE THE FUNCTIONS
      FRGEPS=EPSGLU
      CALL DFNPRE(WGLU,YGLU,XLOW,XHIGH)
      FRGEPS=EPSLQU
      CALL DFNPRE(WLQU,YLQU,XLOW,XHIGH)
      DO 10 I=1,5
        FRGEPS=EPSHVY(I)
   10 CALL DFNPRE(WHVY,YHVY(1,I),XLOW,XHIGH)
      RETURN
      END
