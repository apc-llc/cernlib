*
* $Id: isajfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isajfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAJFL
C-------------------------------------------------------------------
C-
C-  Fill ISAJ Zebra banks (primary partons)
C-
C-  INPUT:
C-
C-  OUTPUT:
C-  NJT  = number of partons
C-
C-      SDP JAN.,1986
C-
C--------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "keys.inc"
#include "pjets.inc"
#include "primar.inc"
#include "zebcom.inc"
#include "isalnk.inc"
      INTEGER I,IOISAJ,LISAJ
      REAL PHI1,THETA,ETA
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(NJET.NE.0) THEN
C
        IF(KEYS(3))  THEN  ! special section for Drell-Yan
          CALL BKISAJ(LISAJ)
          PQREF(1)=LISAJ
          PQREF(2)=LISAJ
          IQ(LISAJ+1)=IDENTW !    particle id
          CALL UCOPY(QWJET,Q(LISAJ+2),5)   ! momenta and masses
          CALL ISPETA(QWJET,THETA,PHI1,ETA)   ! calculate phi,theta and eta
          Q(LISAJ+7)=PHI1
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
        ENDIF
C
        DO 200 I=1,NJET       ! loop over primary jets
        IF((.NOT.KEYS(3).OR.I.EQ.3).AND.      ! only 3rd jet for Drell-Yan
     $     (PJETS(4,I).GT.0.01))         THEN ! and check jet energy
          CALL BKISAJ(LISAJ)
          PQREF(I)=LISAJ
          IQ(LISAJ+1)=IDJETS(I)     !    particle id
          CALL UCOPY(PJETS(1,I),Q(LISAJ+2),5) ! momenta and mass
C   calculate phi,theta and eta
          CALL ISPETA(PJETS(1,I),THETA,PHI1,ETA)
          Q(LISAJ+7)=PHI1
          Q(LISAJ+8)=THETA
          Q(LISAJ+9)=ETA
C
         ENDIF
  200    CONTINUE
C
      ENDIF
      RETURN
      END
