*
* $Id: gefla.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: gefla.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE GEFLA(IX,IFLA,JFLA)
C     ******************************
C-- GENERATION OF INITIAL PARTON FLAVOR - CALLED BY INPART
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
#include "cojets/tabqrk.inc"
      TF=0.
      RTF=CJRN(0)*TABQRK(IX,6)
      DO 115 J=1,5
      TF=TF+TABQRK(IX,J)
      JOUT=J
      IF(RTF.LT.TF) GO TO 116
115   CONTINUE
116   IFLA=IFCODE(JOUT)
      JFLA=JOUT
      IF(IFLA.EQ.3.AND.CJRN(0).GT..5) IFLA=-IFLA
      RETURN
      END
