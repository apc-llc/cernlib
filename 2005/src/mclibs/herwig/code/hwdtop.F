CDECK  ID>, HWDTOP.
*CMZ :-        -09/12/92  11.03.46  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWDTOP(DECAY)
C-----------------------------------------------------------------------
C     DECIDES WHETHER TO DO TOP QUARK DECAY BEFORE HADRONIZATION
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      LOGICAL DECAY
      DECAY=RMASS(6).GT.130D0
      END
