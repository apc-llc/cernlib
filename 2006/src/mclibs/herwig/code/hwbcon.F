CDECK  ID>, HWBCON.
*CMZ :-        -26/04/91  10.18.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWBCON
C-----------------------------------------------------------------------
C     MAKES COLOUR CONNECTIONS BETWEEN JETS
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER IHEP,IST,ID,JC,JD,JHEP,LHEP
      IF (IERROR.NE.0) RETURN
      DO 20 IHEP=1,NHEP
      IST=ISTHEP(IHEP)
C---LOOK FOR PARTONS WITHOUT COLOUR MOTHERS
      IF (IST.LT.145.OR.IST.GT.152) GOTO 20
      IF (JMOHEP(2,IHEP).EQ.0) THEN
C---FIND COLOUR-CONNECTED PARTON
        JC=JMOHEP(1,IHEP)
        IF (IST.NE.152) JC=JMOHEP(1,JC)
        JC =JMOHEP(2,JC)
C---FIND SPECTATOR WHEN JC IS DECAYED HEAVY QUARK
        IF (ISTHEP(JC).EQ.155) THEN
          IF (IDHEP(JMOHEP(1,JC)).EQ.94) THEN
C---QUARK DECAYED BEFORE HADRONIZING
            JHEP=JMOHEP(2,JC)
            ID=IDHW(JHEP)
            IF (ISTHEP(JHEP).EQ.155) THEN
              JC=JDAHEP(2,JHEP)
            ELSEIF (ID.EQ.6.OR.ID.EQ.12.OR.
     &              (ID.GE.209.AND.ID.LE.218)) THEN
C Wait for partner heavy quark to decay
              RETURN
            ELSE
              JMOHEP(2,IHEP)=JHEP
              JDAHEP(2,JHEP)=IHEP
              GOTO 20
            ENDIF
          ELSE
            JC=JMOHEP(2,JC)
          ENDIF
        ENDIF
        JC=JDAHEP(1,JC)
        JD=JDAHEP(2,JC)
C---SEARCH IN CORRESPONDING JET
        IF (JD.LT.JC) JD=JC
        LHEP=0
        DO 10 JHEP=JC,JD
        IF (ISTHEP(JHEP).LT.145.OR.ISTHEP(JHEP).GT.152) GOTO 10
        IF (JDAHEP(2,JHEP).EQ.IHEP) LHEP=JHEP
        IF (JDAHEP(2,JHEP).NE.0) GOTO 10
C---JOIN IHEP AND JHEP
        JMOHEP(2,IHEP)=JHEP
        JDAHEP(2,JHEP)=IHEP
        GOTO 20
   10   CONTINUE
        IF (LHEP.NE.0) THEN
          JMOHEP(2,IHEP)=LHEP
        ELSE
C---COULDN'T FIND PARTNER OF IHEP
          CALL HWWARN('HWBCON',100,*999)
        ENDIF
      ENDIF
  20  CONTINUE
C---BREAK COLOUR CONNECTIONS WITH PHOTONS
      IHEP=1
  30  IF (IHEP.LE.NHEP) THEN
        IF (IDHW(IHEP).EQ.59 .AND. ISTHEP(IHEP).EQ.149) THEN
          IF (JDAHEP(2,JMOHEP(2,IHEP)).EQ.IHEP)
     &      JDAHEP(2,JMOHEP(2,IHEP))=JDAHEP(2,IHEP)
          IF (JDAHEP(2,IHEP).NE.0) THEN
            IF (JMOHEP(2,JDAHEP(2,IHEP)).EQ.IHEP)
     &        JMOHEP(2,JDAHEP(2,IHEP))=JMOHEP(2,IHEP)
          ENDIF
          JMOHEP(2,IHEP)=IHEP
          JDAHEP(2,IHEP)=IHEP
        ENDIF
        IHEP=IHEP+1
        GOTO 30
      ENDIF
  999 END
