*
* $Id: hwcgsp.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwcgsp.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWCGSP.
*CMZ :-        -13/07/92  20.15.54  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWCGSP
C     SPLITS ANY TIMELIKE GLUONS REMAINING AFTER PERTURBATIVE
C     BRANCHING INTO LIGHT (I.E. U OR D) Q-QBAR PAIRS
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER HWRINT,IHEP,JHEP,KHEP,LHEP,MHEP,ID,J,IST
      DOUBLE PRECISION HWRGEN,PF
      IF (NGSPL.EQ.0) CALL HWWARN('HWCGSP',400,*999)
      LHEP=NHEP-1
      MHEP=NHEP
      DO 10 IHEP=1,NHEP
      IF (ISTHEP(IHEP).GE.147.AND.ISTHEP(IHEP).LE.149) THEN
        JHEP=JMOHEP(2,IHEP)
C---CHECK FOR DECAYED HEAVY ANTIQUARKS
        IF (ISTHEP(JHEP).EQ.155) THEN
          JHEP=JDAHEP(1,JDAHEP(2,JHEP))
          DO 1 J=JDAHEP(1,JHEP),JDAHEP(2,JHEP)
          IF (ISTHEP(J).EQ.149.AND.JDAHEP(2,J).EQ.0) GO TO 2
    1     CONTINUE
          CALL HWWARN('HWCGSP',100,*999)
    2     JHEP=J
        ENDIF
        KHEP=JDAHEP(2,IHEP)
C---CHECK FOR DECAYED HEAVY QUARKS
        IF (ISTHEP(KHEP).EQ.155)  CALL HWWARN('HWCGSP',101,*999)
        IF (IDHW(IHEP).EQ.13) THEN
C---SPLIT A GLUON
          LHEP=LHEP+2
          MHEP=MHEP+2
    5     ID=HWRINT(1,NGSPL)
          IF (PGSPL(ID).LT.PGSMX*HWRGEN(0)) GO TO 5
          PHEP(5,LHEP)=RMASS(ID)
          PHEP(5,MHEP)=RMASS(ID)
C---ASSUME ISOTROPIC ANGULAR DISTRIBUTION
          IF (PHEP(5,IHEP).GT.PHEP(5,LHEP)+PHEP(5,MHEP)) THEN
            CALL HWDTWO(PHEP(1,IHEP),PHEP(1,LHEP),
     &                  PHEP(1,MHEP),PGSPL(ID),TWO,.TRUE.)
          ELSE
            PF=HWRGEN(1)
            CALL HWVSCA(4,PF,PHEP(1,IHEP),PHEP(1,LHEP))
            CALL HWVDIF(4,PHEP(1,IHEP),PHEP(1,LHEP),PHEP(1,MHEP))
            CALL HWUMAS(PHEP(1,LHEP))
            CALL HWUMAS(PHEP(1,MHEP))
          ENDIF
          IDHW(LHEP)=ID+6
          IDHW(MHEP)=ID
          IDHEP(MHEP)= IDPDG(ID)
          IDHEP(LHEP)=-IDPDG(ID)
          ISTHEP(IHEP)=2
          ISTHEP(LHEP)=150
          ISTHEP(MHEP)=150
C---NEW COLOUR CONNECTIONS
          JMOHEP(2,KHEP)=LHEP
          JDAHEP(2,JHEP)=MHEP
          JMOHEP(1,LHEP)=JMOHEP(1,IHEP)
          JMOHEP(2,LHEP)=MHEP
          JMOHEP(1,MHEP)=JMOHEP(1,IHEP)
          JMOHEP(2,MHEP)=JHEP
          JDAHEP(1,LHEP)=0
          JDAHEP(2,LHEP)=KHEP
          JDAHEP(1,MHEP)=0
          JDAHEP(2,MHEP)=LHEP
          JDAHEP(1,IHEP)=LHEP
          JDAHEP(2,IHEP)=MHEP
        ELSE
C---COPY A NON-GLUON
          LHEP=LHEP+1
          MHEP=MHEP+1
          CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,MHEP))
          IDHW(MHEP)=IDHW(IHEP)
          IDHEP(MHEP)=IDHEP(IHEP)
          IST=ISTHEP(IHEP)
          ISTHEP(IHEP)=2
          IF (IST.EQ.149) THEN
            ISTHEP(MHEP)=150
          ELSE
            ISTHEP(MHEP)=IST+6
          ENDIF
C---NEW COLOUR CONNECTIONS
          JMOHEP(2,KHEP)=MHEP
          JDAHEP(2,JHEP)=MHEP
          JMOHEP(1,MHEP)=JMOHEP(1,IHEP)
          JMOHEP(2,MHEP)=JMOHEP(2,IHEP)
          JDAHEP(1,MHEP)=0
          JDAHEP(2,MHEP)=JDAHEP(2,IHEP)
          JDAHEP(1,IHEP)=MHEP
        ENDIF
      ENDIF
   10 CONTINUE
      NHEP=MHEP
  999 END
