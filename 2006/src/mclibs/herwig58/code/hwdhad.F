*
* $Id: hwdhad.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdhad.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDHAD.
*CMZ :-        -26/04/91  14.01.26  by  Federico Carminati
*-- Author :    Bryan Webber & Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWDHAD
C     GENERATES DECAYS OF UNSTABLE HADRONS AND LEPTONS
C-----------------------------------------------------------------------
C     MODIFICATIONS FROM ORIGINAL (BY MHS):
C     IF HARD CM IS A COLOUR SINGLET, COPY IT READY FOR DECAY
C     IF UNDECAYED  H  FOUND, CALL HWDHIG
C     IF UNDECAYED W/Z FOUND, CALL HWDBOS
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER KHEP,LHEP,MHEP,ID,NM,IMAD,IMIN,IMAX,IM,IST,ID1,ID2,ID3,
     &  MO,I
      DOUBLE PRECISION HWRGEN,RN,BF,HWDPWT,HWDWWT,COSANG,RSUM
      EXTERNAL HWDPWT,HWDWWT,HWRGEN
      IF (IERROR.NE.0) RETURN
      DO 100 KHEP=1,NMXHEP
      IF (KHEP.GT.NHEP) THEN
        ISTAT=90
        RETURN
      ELSEIF (ISTHEP(KHEP).EQ.120 .AND.
     &  JDAHEP(1,KHEP).EQ.KHEP .AND. JDAHEP(2,KHEP).EQ.KHEP) THEN
C---COPY COLOUR SINGLET CMF
        NHEP=NHEP+1
        IF (NHEP.GT.NMXHEP) CALL HWWARN('HWDHAD',100,*999)
        CALL HWVEQU(5,PHEP(1,KHEP),PHEP(1,NHEP))
        IDHW(NHEP)=IDHW(KHEP)
        IDHEP(NHEP)=IDHEP(KHEP)
        ISTHEP(NHEP)=190
        JMOHEP(1,NHEP)=KHEP
        JMOHEP(2,NHEP)=NHEP
        JDAHEP(2,NHEP)=NHEP
        JDAHEP(1,KHEP)=NHEP
        JDAHEP(2,KHEP)=NHEP
      ELSEIF (ISTHEP(KHEP).GT.189.AND.ISTHEP(KHEP).LT.195) THEN
C---FIRST CHECK FOR STABILITY
        ID=IDHW(KHEP)
        NM=MODEF(ID)
        IF (NM.EQ.0) THEN
          ISTHEP(KHEP)=1
          JDAHEP(1,KHEP)=0
          JDAHEP(2,KHEP)=0
C---SPECIAL FOR GAUGE BOSON DECAY
          IF (ID.GE.198.AND.ID.LE.200) CALL HWDBOS(KHEP)
C---SPECIAL FOR HIGGS BOSON DECAY
          IF (ID.EQ.201) CALL HWDHIG(ZERO)
        ELSE
C---UNSTABLE.  CHOOSE DECAY MODE
          ISTHEP(KHEP)=ISTHEP(KHEP)+5
          RN=HWRGEN(0)
          BF=0.
          IMAD=MADDR(ID)
          IMIN=IMAD+1
          IMAX=IMAD+NM
          DO 40 IM=IMIN,IMAX
          BF=BF+BFRAC(IM)
          IF (BF.GE.RN) GO TO 50
   40     CONTINUE
          IM=IMAX
C---FIND DECAY PRODUCTS
   50     ID1=IDPRO(1,IM)
          ID2=IDPRO(2,IM)
          ID3=IDPRO(3,IM)
          IST=193
          NHEP=NHEP+1
          IF (NHEP.GT.NMXHEP) CALL HWWARN('HWDHAD',101,*999)
          IDHW(NHEP)=ID1
          IDHEP(NHEP)=IDPDG(ID1)
          ISTHEP(NHEP)=IST
          JMOHEP(1,NHEP)=KHEP
          JMOHEP(2,NHEP)=JMOHEP(2,KHEP)
          JDAHEP(1,KHEP)=NHEP
          IF (ID2.EQ.0) THEN
C---ONE BODY DECAY (FOR K0,K0BAR->K0S,K0L)
            CALL HWVEQU(5,PHEP(1,KHEP),PHEP(1,NHEP))
          ELSE
            LHEP=NHEP
            NHEP=NHEP+1
            IF (NHEP.GT.NMXHEP) CALL HWWARN('HWDHAD',102,*999)
            IDHW(NHEP)=ID2
            IDHEP(NHEP)=IDPDG(ID2)
            ISTHEP(NHEP)=IST
            JMOHEP(1,NHEP)=KHEP
            JMOHEP(2,NHEP)=JMOHEP(2,KHEP)
            PHEP(5,LHEP)=RMASS(ID1)
            PHEP(5,NHEP)=RMASS(ID2)
            IF (ID3.EQ.0) THEN
C---SPECIAL TREATMENT OF POLARIZED MESONS
              COSANG=TWO
              IF (ID.EQ.IDHW(JMOHEP(1,KHEP))) THEN
                MO=JMOHEP(1,KHEP)
                RSUM=0
                DO 60 I=1,3
 60             RSUM=RSUM+RHOHEP(I,MO)
                IF (RSUM.GT.0) THEN
                  RSUM=RSUM*HWRGEN(0)
                  IF (RSUM.LT.RHOHEP(1,MO)) THEN
C---(1+COSANG)**2
                    COSANG=MAX(HWRGEN(0),HWRGEN(1),HWRGEN(2))*TWO-ONE
                  ELSEIF (RSUM.LT.RHOHEP(1,MO)+RHOHEP(2,MO)) THEN
C---1-COSANG**2
                    COSANG=2*COS((ACOS(HWRGEN(0)*TWO-ONE)+PIFAC)/THREE)
                  ELSE
C---(1-COSANG)**2
                    COSANG=MIN(HWRGEN(0),HWRGEN(1),HWRGEN(2))*TWO-ONE
                  ENDIF
                ENDIF
              ENDIF
C---TWO BODY DECAY
              CALL HWDTWO(PHEP(1,KHEP),PHEP(1,LHEP),
     &                    PHEP(1,NHEP),CMMOM(IM),COSANG,.FALSE.)
            ELSE
C---THREE BODY DECAY
              MHEP=NHEP
              NHEP=NHEP+1
              IF (NHEP.GT.NMXHEP) CALL HWWARN('HWDHAD',103,*999)
              IDHW(NHEP)=ID3
              IDHEP(NHEP)=IDPDG(ID3)
              ISTHEP(NHEP)=IST
              JMOHEP(1,NHEP)=KHEP
              JMOHEP(2,NHEP)=JMOHEP(2,KHEP)
              PHEP(5,NHEP)=RMASS(ID3)
C---USE V-A MATRIX ELEMENT FOR WEAK LEPTONIC DECAYS
              IF (ID .GT.120.AND.ID .LT.133.AND.
     &            ID1.GT.120.AND.ID1.LT.133) THEN
                CALL HWDTHR(PHEP(1,KHEP),PHEP(1,LHEP),PHEP(1,MHEP),
     &                      PHEP(1,NHEP),HWDWWT)
              ELSE
                CALL HWDTHR(PHEP(1,KHEP),PHEP(1,LHEP),PHEP(1,MHEP),
     &                      PHEP(1,NHEP),HWDPWT)
              ENDIF
            ENDIF
          ENDIF
          JDAHEP(2,KHEP)=NHEP
        ENDIF
      ENDIF
  100 CONTINUE
C---MAY HAVE OVERFLOWED /HEPEVT/
      CALL HWWARN('HWDHAD',104,*999)
  999 END
