*
* $Id: hwbcon.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbcon.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBCON.
*CMZ :-        -26/04/91  10.18.56  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWBCON
C     MAKES COLOUR CONNECTIONS BETWEEN JETS
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER IHEP,IST,JC,JD,JHEP,LHEP
      IF (IERROR.NE.0) RETURN
      DO 50 IHEP=1,NHEP
      IST=ISTHEP(IHEP)
C---LOOK FOR PARTONS WITHOUT COLOUR MOTHERS
      IF (IST.LT.145.OR.IST.GT.152) GO TO 50
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
            IF (ISTHEP(JHEP).EQ.155) THEN
              JC=JDAHEP(1,JDAHEP(2,JHEP))
            ELSE
              JMOHEP(2,IHEP)=JHEP
              JDAHEP(2,JHEP)=IHEP
              GO TO 30
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
        DO 20 JHEP=JC,JD
        IF (ISTHEP(JHEP).LT.145) GO TO 20
        IF (ISTHEP(JHEP).GT.152) GO TO 20
        IF (JDAHEP(2,JHEP).EQ.IHEP) LHEP=JHEP
        IF (JDAHEP(2,JHEP).NE.0) GO TO 20
C---JOIN IHEP AND JHEP
        JMOHEP(2,IHEP)=JHEP
        JDAHEP(2,JHEP)=IHEP
        GO TO 30
   20   CONTINUE
        IF (LHEP.NE.0) THEN
          JMOHEP(2,IHEP)=LHEP
        ELSE
C---COULDN'T FIND PARTNER OF IHEP
          CALL HWWARN('HWBCON',100,*999)
        ENDIF
      ENDIF
   30 CONTINUE
   50 CONTINUE
C---BREAK COLOUR CONNECTIONS WITH PHOTONS
      IHEP=1
 100  IF (IHEP.LE.NHEP) THEN
        IF (IDHW(IHEP).EQ.59 .AND. ISTHEP(IHEP).EQ.149) THEN
          IF (JDAHEP(2,JMOHEP(2,IHEP)).EQ.IHEP)
     &      JDAHEP(2,JMOHEP(2,IHEP))=JDAHEP(2,IHEP)
          IF (JMOHEP(2,JDAHEP(2,IHEP)).EQ.IHEP)
     &      JMOHEP(2,JDAHEP(2,IHEP))=JMOHEP(2,IHEP)
          JMOHEP(2,IHEP)=IHEP
          JDAHEP(2,IHEP)=IHEP
        ENDIF
        IHEP=IHEP+1
        GOTO 100
      ENDIF
  999 END
