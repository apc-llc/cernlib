*
* $Id: hwsgqq.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwsgqq.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWSGQQ.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWSGQQ(QSCA)
C     CORRECTION TO GLUON STRUCTURE FUNCTION FOR BACKWARD EVOLUTION:
C     G->Q-QBAR PART OF FORM FACTOR
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION QSCA,GG,HWSGQQ,HWUALF
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
