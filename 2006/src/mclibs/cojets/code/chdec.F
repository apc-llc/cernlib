*
* $Id: chdec.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: chdec.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE CHDEC(IPIN,IDC,CH)
C     *****************************
C-- GIVES DECAY SECONDARIES LABELS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/data3.inc"
#include "cojets/itapes.inc"
      CHARACTER*8 LABEL
      CHARACTER*5 CH(5)
      CHARACTER*5 BLANK
C
      DATA BLANK/' '/
C
      DO 6 J=1,5
      JJ=KDP(IDC,J)
      IF(JJ.EQ.0) THEN
        CH(J)=BLANK
      ELSE
        IF(IPIN.NE.4) CH(J)=LABEL(IDEXT(JJ))
        IF(IPIN.EQ.4) CH(J)=LABEL(-IDEXT(JJ))
      ENDIF
6     CONTINUE
C
      RETURN
      END
