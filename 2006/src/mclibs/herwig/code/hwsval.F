CDECK  ID>, HWSVAL.
*CMZ :-        -26/04/91  10.18.58  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWSVAL(ID)
C-----------------------------------------------------------------------
C     TRUE FOR VALENCE PARTON ID IN INCOMING HADRON INHAD
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER ID,IDHAD
      LOGICAL HWSVAL
      HWSVAL=.FALSE.
      IDHAD=IDHW(INHAD)
      IF (IDHAD.EQ.73.OR.IDHAD.EQ.75) THEN
        IF (ID.EQ.1.OR.ID.EQ.2) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.91.OR.IDHAD.EQ.93) THEN
        IF (ID.EQ.7.OR.ID.EQ.8) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.30) THEN
        IF (ID.EQ.1.OR.ID.EQ.8) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.38) THEN
        IF (ID.EQ.2.OR.ID.EQ.7) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.59) THEN
        IF (ID.LT.6.OR.(ID.GT.6.AND.ID.LT.12)) HWSVAL=.TRUE.
      ELSEIF (IDHAD.EQ.71.OR.IDHAD.EQ.72) THEN
        IF (ID.EQ.13) HWSVAL=.TRUE.
      ELSE
        CALL HWWARN('HWSVAL',100,*999)
      ENDIF
  999 END
