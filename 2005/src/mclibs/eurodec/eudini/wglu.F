*
* $Id: wglu.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: wglu.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION WGLU(X)
C.----------------------------------------------------------------------
C.
C.    FIELD-FEYNMAN FRAGMENTATION FUNCTION FOR GLUON
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION X,WGLU
#endif
#include "eurodec/epsfrg.inc"
      WGLU=X**2+(1.0-X)**2
      RETURN
      END
