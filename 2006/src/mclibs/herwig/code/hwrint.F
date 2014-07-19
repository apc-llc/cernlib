CDECK  ID>, HWRINT.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWRINT(IMIN,IMAX)
C-----------------------------------------------------------------------
C     RANDOM INTEGER IN [IMIN,IMAX]. N.B. ASSUMES IMAX.GE.IMIN
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,RN
      INTEGER HWRINT,IMIN,IMAX
      EXTERNAL HWRGEN
    1 RN=HWRGEN(0)
      IF (RN.EQ.1.) GOTO 1
      RN=RN*(IMAX-IMIN+1)
      HWRINT=IMIN+INT(RN)
      END
