*
* $Id: isalfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isalfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISALFL(NLEP)
C-------------------------------------------------------------------
C-
C-      Fill Zebra banks  ISAL, same format as ISP1 but only
C-      leptons with reference links to ISP1, ISAQ, ISAJ
C-      Needs banks ISP1 and ISV1
C-
C-      OUTPUT:
C-      NLEP = number of leptons
C-
C-                   SDP May,1986
C-
C---------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "izisal.inc"
#include "izisp1.inc"
#include "izisv1.inc"
#include "zebcom.inc"
      INTEGER LISV1,LISP1,LISAL,IOISAL,JBIAS,IDABS
      INTEGER NLEP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        CALL MZFORM('ISAL','1I -F',IOISAL)
        FIRST=.FALSE.
      ENDIF
C
C        set pointers for first lepton bank
C
      JBIAS=-IZISAL
      LISAL=LISAE
      NLEP=0
C
C          Find leptons
C
      LISV1=LISAE-IZISV1
C
  100 LISV1=LQ(LISV1)        ! loop over vertices
      IF(LISV1.GT.0) THEN
        LISP1=LISV1-IZISP1
C
  200   LISP1=LQ(LISP1)      ! loop over particles
        IF(LISP1.GT.0) THEN
          IDABS=IABS(IQ(LISP1+1))
          IF(IDABS.LT.11.OR.IDABS.GT.15) GOTO 200
C
          CALL MZBOOK(IXMAIN,LISAL,LISAL,JBIAS,
     $               'ISAL',4,1,9,IOISAL,-1)
          CALL UCOPY(Q(LISP1+1),Q(LISAL+1),9)   ! same information as in ISP1
          LQ(LISAL-4)=LISP1         ! reference link to ISP1
          LQ(LISAL-2)=LQ(LISP1-2)   ! reference link to ISAQ
          LQ(LISAL-3)=LQ(LISP1-3)   ! reference link to ISAJ
          NLEP=NLEP+1
C
          JBIAS=0
          GOTO 200
        ENDIF
C
      GOTO 100
      ENDIF
      RETURN
      END
