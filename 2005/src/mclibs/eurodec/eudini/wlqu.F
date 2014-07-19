*
* $Id: wlqu.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: wlqu.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION WLQU(X)
C.----------------------------------------------------------------------
C.
C.    FIELD-FEYNMAN FRAGMENTATION FUNCTION FOR LIGHT QUARKS
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION X,WLQU
#endif
#include "eurodec/epsfrg.inc"
      WLQU=1.0-FRGEPS+3.0*FRGEPS*(1.0-X)**2
      RETURN
      END
