*
* $Id: epuref.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: epuref.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPUREF(LUNIT)
C. RETURNS INTERNAL REFERENCE NUMBER FOR LOGICAL UNIT.
C.
C. INPUT
C. LUNIT     LOGICAL UNIT NUMBER
C.-- OUTPUT IN COMMON BLOCK :
C   LREF     INTERNAL UNIT NUMBER
C   ISTART   START OF UNIT DESCRIPTOR IN LIST
#include "epio/epiocom.inc"
      LASTUT=LUNIT
      NC=0
      N=LIST(1)
      IF(N.EQ.0)  GOTO 77777
      K=NCONT+10
      DO 1  NC=1,N
      IF(LUNIT.EQ.LIST(K))  GOTO 77777
      K=K+NWUNIT
    1 CONTINUE
      NC=0
77777 LREF=NC
      ISTART=NCONT+(NC-1)*NWUNIT
      RETURN
      END
