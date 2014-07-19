CDECK  ID>, HWBVMC.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWBVMC(ID)
C-----------------------------------------------------------------------
C     VIRTUAL MASS CUTOFF FOR PARTON TYPE ID
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWBVMC
      INTEGER ID
      IF (ID.EQ.13) THEN
        HWBVMC=RMASS(ID)+VGCUT
      ELSEIF (ID.LT.13) THEN
        HWBVMC=RMASS(ID)+VQCUT
      ELSEIF (ID.EQ.59) THEN
        HWBVMC=RMASS(ID)+VPCUT
      ELSE
        HWBVMC=RMASS(ID)
      ENDIF
      END
