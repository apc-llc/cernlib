*
* $Id: tzacw.F,v 1.2 1999/06/18 13:31:32 couet Exp $
*
* $Log: tzacw.F,v $
* Revision 1.2  1999/06/18 13:31:32  couet
* - qcardl.inc was empty: It is now removed and not used.
*   Comment END CDE removed.
*
* Revision 1.1.1.1  1996/03/06 10:47:27  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE TZACW (TEXT)

C-    Krack  A[n][C][W]  for TZINIT and TZFREE

#include "zebra/zmach.inc"
#include "zebra/tzc1.inc"
*
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,DUMMY(36)
      CHARACTER    TEXT*(*)
      DIMENSION    IFLHOL(2)
      EQUIVALENCE (IFLHOL(1),IFLHC)


      JE = LEN (TEXT)
      IF (JE.EQ.1)  THEN
          NCHPW = IQCHAW
          RETURN
        ENDIF

      NCHPW = ICDECI (TEXT,2,JE)
      JA    = NESLAT
      IF (NCHPW.EQ.0)  NCHPW = IQCHAW
      NCHPW = MIN (NCHPW,IQCHAW)
      IF (JA.GT.JE)                RETURN
      CALL CLTOU (TEXT)

   24 J = INDEX('CW',TEXT(JA:JA))
      IF (J.EQ.0)                  GO TO 92
      IFLHOL(J) = 1
      JA = JA + 1
      IF (JA.GT.JE)                RETURN
      GO TO 24

C--       Trouble

   92 NCHPW = -JA
      RETURN
      END
