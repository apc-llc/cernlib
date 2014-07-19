CDECK  ID>, HWUPCM.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWUPCM(EM0,EM1,EM2)
C-----------------------------------------------------------------------
C     C.M. MOMENTUM FOR DECAY MASSES EM0 -> EM1 + EM2
C     SET TO -1 BELOW THRESHOLD
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWUPCM,EM0,EM1,EM2,EMS,EMD
      EMS=ABS(EM1+EM2)
      EMD=ABS(EM1-EM2)
      IF (EM0.LT.EMS.OR.EM0.LT.EMD) THEN
        HWUPCM=-1.
      ELSEIF (EM0.EQ.EMS.OR.EM0.EQ.EMD) THEN
        HWUPCM=0.
      ELSE
        HWUPCM=SQRT((EM0+EMD)*(EM0-EMD)*
     &              (EM0+EMS)*(EM0-EMS))*.5/EM0
      ENDIF
      END
