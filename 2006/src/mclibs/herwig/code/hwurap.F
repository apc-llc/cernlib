CDECK  ID>, HWURAP.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWURAP(P)
C-----------------------------------------------------------------------
C     LONGITUDINAL RAPIDITY (SET TO +/-1000 IF TOO LARGE)
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWURAP,EMT2,P(5)
      EMT2=P(1)**2+P(2)**2+P(5)**2
      IF (P(3).GT.0.) THEN
        IF (EMT2.EQ.0.) THEN
          HWURAP=1000.
        ELSE
          HWURAP= 0.5*LOG((P(3)+P(4))**2/EMT2)
        ENDIF
      ELSEIF (P(3).LT.0.) THEN
        IF (EMT2.EQ.0.) THEN
          HWURAP=-1000.
        ELSE
          HWURAP=-0.5*LOG((P(3)-P(4))**2/EMT2)
        ENDIF
      ELSE
          HWURAP=0.
      ENDIF
      END
