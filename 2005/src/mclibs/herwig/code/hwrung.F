CDECK  ID>, HWRUNG.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    David Ward, modified by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWRUNG(A,B)
C-----------------------------------------------------------------------
C     Random number from distribution having flat top [-A,A] & gaussian
C     tail of s.d. B
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRUNG,HWRGAU,HWRUNI,A,B,PRUN,ZERO
      LOGICAL HWRLOG
      EXTERNAL HWRGAU,HWRUNI,HWRLOG
      PARAMETER (ZERO=0.D0)
      IF (A.EQ.0) THEN
        PRUN=0
      ELSE
        PRUN=1./(1.+B*1.2533/A)
      ENDIF
      IF(HWRLOG(PRUN)) THEN
        HWRUNG=HWRUNI(0,-A,A)
      ELSE
        HWRUNG=HWRGAU(0,ZERO,B)
        HWRUNG=HWRUNG+SIGN(A,HWRUNG)
      ENDIF
      END
