*
* $Id: isaqfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isaqfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAQFL
C-------------------------------------------------------------------
C-
C-  Fill ISAQ Zebra banks (initial and final partons)
C-
C-      SDP JAN.,1986
C-   Updated  12-DEC-1989   Serban D. Protopopescu
C-
C--------------------------------------------------------------------
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "jetset.inc"
#include "zebcom.inc"
#include "isalnk.inc"
      INTEGER I,LISAQ,JOR
      REAL PHI1,THETA,ETA
C--------------------------------------------------------------------
C
      LISAQ=0
C
      IF(NJSET.NE.0) THEN
C
        DO 200 I=1,NJSET       ! loop over all partons
C   keep only stable partons and skip ones with 0 energy
C   skip also t-quarks as decay products will be added later
          IF(JDCAY(I).EQ.0.AND.PJSET(4,I).GT.0.
     &      .AND.IABS(JTYPE(I)).NE.6) THEN
            JOR=JORIG(I)/JPACK
            CALL BKISAQ(LISAQ)
            QREF(I)=LISAQ
            LQ(LISAQ-1)=0      !  reference link is 0 for initial partons
            IF(JOR.LT.10)
     $        LQ(LISAQ-1)=PQREF(JOR)
C   fill Zebra bank
            IQ(LISAQ+1)=JTYPE(I)                  ! parton type
            CALL UCOPY(PJSET(1,I),Q(LISAQ+2),5)   ! momenta and mass
C   calculate PHI1,theta and eta
            CALL ISPETA(PJSET(1,I),THETA,PHI1,ETA)
            Q(LISAQ+7)=PHI1
            Q(LISAQ+8)=THETA
            Q(LISAQ+9)=ETA
          ENDIF
  200   CONTINUE
C
      ENDIF
      RETURN
      END
