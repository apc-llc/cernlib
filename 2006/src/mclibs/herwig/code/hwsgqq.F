CDECK  ID>, HWSGQQ.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWSGQQ(QSCA)
C-----------------------------------------------------------------------
C     CORRECTION TO GLUON STRUCTURE FUNCTION FOR BACKWARD EVOLUTION:
C     G->Q-QBAR PART OF FORM FACTOR
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWSGQQ,HWUALF,QSCA,GG
      EXTERNAL HWUALF
      GG=HWUALF(1,QSCA)**(-1.D0/BETAF)
      IF (GG.LT.1.D0) GG=1.D0
      IF (QSCA.GT.RMASS(6)) THEN
        HWSGQQ=GG**6
      ELSEIF (QSCA.GT.RMASS(5)) THEN
        HWSGQQ=GG**5
      ELSEIF (QSCA.GT.RMASS(4)) THEN
        HWSGQQ=GG**4
      ELSE
        HWSGQQ=GG**3
      ENDIF
      END
