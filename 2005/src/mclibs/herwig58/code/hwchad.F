*
* $Id: hwchad.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwchad.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWCHAD.
*CMZ :-        -26/04/91  14.00.57  by  Federico Carminati
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWCHAD(JCL,ID1,ID3,ID2)
C     HADRONIZES CLUSTER JCL, CONSISTING OF PARTONS ID1,ID3
C
C     ID2 RETURNS PARTON-ANTIPARTON PAIR CREATED
C     (IN SPECIAL CLUSTER CODE - SEE HWCFLA)
C------------------------------------------------------------------------
C     MODIFIED 18/2/92 BY BRW TO GIVE ANISOTROPIC
C     DECAY OF PERTURBATIVE QUARK CLUSTERS
C     MODIFIED 10/8/94 BY BRW TO INCLUDE GAUSSIAN SMEARING (CLSMR)
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER HWRINT,JCL,ID1,ID2,ID3,ID,IR1,IR2,NTRY,IDMIN,IMAX,I,LOC,
     & MHEP,IM,JM,KM
      DOUBLE PRECISION HWRGEN,EM0,EM1,EM2,EMADU,EMSQ,PCMAX,RES,PCM,
     & PTEST,PCQK,PP(5),EMLOW,RMAT(3,3),CT,ST,CX,SX
      LOGICAL DIQK
      DIQK(ID)=ID.GT.3.AND.ID.LT.10
      IF (IERROR.NE.0) RETURN
      ID2=0
      EM0=PHEP(5,JCL)
      IR1=LOCN(ID1,ID3)
      EM1=RMASS(IR1)
      IF (ABS(EM0-EM1).LT.0.001) THEN
C---SINGLE-HADRON CLUSTER
        NHEP=NHEP+1
        IF (NHEP.GT.NMXHEP) CALL HWWARN('HWCHAD',100,*999)
        IDHW(NHEP)=IR1
        IDHEP(NHEP)=IDPDG(IR1)
        ISTHEP(NHEP)=191
        JDAHEP(1,JCL)=NHEP
        JDAHEP(2,JCL)=NHEP
        CALL HWVEQU(5,PHEP(1,JCL),PHEP(1,NHEP))
      ELSE
        NTRY=0
        IDMIN=1
        EMLOW=RMIN(ID1,1)+RMIN(1,ID3)
        EMADU=RMIN(ID1,2)+RMIN(2,ID3)
        IF (EMADU.LT.EMLOW) THEN
          IDMIN=2
          EMLOW=EMADU
        ENDIF
        EMSQ=EM0**2
        PCMAX=EMSQ-EMLOW**2
        IF (PCMAX.GE.0.) THEN
C---SET UP TWO QUARK-ANTIQUARK PAIRS OR A
C   QUARK-DIQUARK AND AN ANTIDIQUARK-ANTIQUARK
          PCMAX=PCMAX*(EMSQ-(RMIN(ID1,IDMIN)-RMIN(IDMIN,ID3))**2)
          IMAX=12
          IF (DIQK(ID1).OR.DIQK(ID3)) IMAX=3
          DO 125 I=3,IMAX
          IF (EM0.LT.RMIN(ID1,I)+RMIN(I,ID3)) GO TO 130
  125     CONTINUE
          I=IMAX+1
  130     ID2=HWRINT(1,I-1)
          IF (PWT(ID2).NE.1.) THEN
            IF (PWT(ID2).LT.HWRGEN(1)) GO TO 130
          ENDIF
C---PICK TWO PARTICLES WITH THESE QUANTUM NUMBERS
          NTRY=NTRY+1
          LOC=LOCN(ID1,ID2)
          RES=RESN(ID1,ID2)
  132     IR1=LOC+INT(RES*HWRGEN(2))
          IF (RESWT(IR1).LT.HWRGEN(3)) GO TO 132
          LOC=LOCN(ID2,ID3)
          RES=RESN(ID2,ID3)
  134     IR2=LOC+INT(RES*HWRGEN(4))
          IF (RESWT(IR2).LT.HWRGEN(5)) GO TO 134
          EM1=RMASS(IR1)
          EM2=RMASS(IR2)
          PCM=EMSQ-(EM1+EM2)**2
          IF (PCM.GT.0.) GO TO 145
  135     IF (NTRY.LE.NDTRY) GO TO 130
C---CAN'T FIND A DECAY MODE - CHOOSE LIGHTEST
  136     ID2=HWRINT(1,2)
          IR1=LOCN(ID1,ID2)
          IR2=LOCN(ID2,ID3)
          EM1=RMASS(IR1)
          EM2=RMASS(IR2)
          PCM=EMSQ-(EM1+EM2)**2
          IF (PCM.GT.0.) GO TO 145
          NTRY=NTRY+1
          IF (NTRY.LE.NDTRY+50) GO TO 136
          CALL HWWARN('HWCHAD',101,*999)
C---DECAY IS ALLOWED
  145     PCM=PCM*(EMSQ-(EM1-EM2)**2)
          IF (NTRY.GT.NCTRY) GO TO 146
          PTEST=PCM*SWTEF(IR1)*SWTEF(IR2)
          IF (PTEST.LT.PCMAX*HWRGEN(0)**2) GO TO 130
        ELSE
C---ALLOW DECAY BY PI0 EMISSION IF ONLY POSSIBILITY
          ID2=1
          IR2=LOCN(1,1)
          EM2=RMASS(IR2)
          PCM=(EMSQ-(EM1+EM2)**2)*(EMSQ-(EM1-EM2)**2)
        ENDIF
C---DECAY IS CHOSEN.  GENERATE DECAY MOMENTA
C   AND PUT PARTICLES IN /HEPEVT/
  146   IF (PCM.LT.0.) CALL HWWARN('HWCHAD',102,*999)
        PCM=0.5*SQRT(PCM)/EM0
        MHEP=NHEP+1
        NHEP=NHEP+2
        IF (NHEP.GT.NMXHEP) CALL HWWARN('HWCHAD',103,*999)
        PHEP(5,MHEP)=EM1
        PHEP(5,NHEP)=EM2
C***MOD FOR ANISOTROPIC DECAY OF PERTURBATIVE QUARK CLUSTERS********
        IF (CLDIR.NE.0) THEN
          DO 150 IM=1,2
            JM=JMOHEP(IM,JCL)
            IF (JM.EQ.0) GO TO 150
            IF (ISTHEP(JM).NE.158) GO TO 150
C   LOOK FOR PARENT PARTON
            DO 149 KM=JMOHEP(1,JM)+1,JM
              IF (ISTHEP(KM).EQ.2) THEN
                IF (JDAHEP(1,KM).EQ.JM) THEN
C   FOUND PARENT PARTON
                  IF (IDHW(KM).NE.13) THEN
C   FIND ITS DIRECTION IN CLUSTER CMF
                   CALL HWULOF(PHEP(1,JCL),PHEP(1,KM),PP)
                   PCQK=PP(1)**2+PP(2)**2+PP(3)**2
                   IF (PCQK.GT.ZERO) THEN
                    PCQK=SQRT(PCQK)
                    IF (CLSMR.GT.ZERO) THEN
C   DO GAUSSIAN SMEARING OF DIRECTION
  147                CT=ONE+CLSMR*LOG(HWRGEN(0))
                     IF (CT.LT.-ONE) GO TO 147
                     ST=ONE-CT*CT
                     IF (ST.GT.ZERO) ST=SQRT(ST)
                     CALL HWRAZM( ONE,CX,SX)
                     CALL HWUROT(PP,CX,SX,RMAT)
                     PP(1)=ZERO
                     PP(2)=PCQK*ST
                     PP(3)=PCQK*CT
                     CALL HWUROB(RMAT,PP,PP)
                    ENDIF
                    PCQK=PCM/PCQK
                    IF (IM.EQ.2) PCQK=-PCQK
                    CALL HWVSCA(3,PCQK,PP,PHEP(1,MHEP))
                    PHEP(4,MHEP)=SQRT(PHEP(5,MHEP)**2+PCM**2)
                    CALL HWULOB(PHEP(1,JCL),PHEP(1,MHEP),PHEP(1,MHEP))
                    CALL HWVDIF(4,PHEP(1,JCL),PHEP(1,MHEP),PHEP(1,NHEP))
                    GO TO 152
                   ENDIF
                  ENDIF
                  GO TO 151
                ENDIF
              ELSEIF (ISTHEP(KM).GT.140) THEN
C   FINISHED THIS JET
                GO TO 150
              ENDIF
 149        CONTINUE
 150      CONTINUE
        ENDIF
 151    CALL HWDTWO(PHEP(1,JCL),PHEP(1,MHEP),PHEP(1,NHEP),
     &              PCM,TWO,.TRUE.)
 152    IDHW(MHEP)=IR1
C***END MOD*********************************************************
        IDHW(NHEP)=IR2
        IDHEP(MHEP)=IDPDG(IR1)
        IDHEP(NHEP)=IDPDG(IR2)
        ISTHEP(MHEP)=192
        ISTHEP(NHEP)=192
        JMOHEP(1,MHEP)=JCL
C---SECOND MOTHER OF HADRON IS JET
        JMOHEP(2,MHEP)=JMOHEP(1,JMOHEP(1,JCL))
        JDAHEP(1,JCL)=MHEP
        JDAHEP(2,JCL)=NHEP
      ENDIF
      ISTHEP(JCL)=180+MOD(ISTHEP(JCL),10)
      JMOHEP(1,NHEP)=JCL
      JMOHEP(2,NHEP)=JMOHEP(1,JMOHEP(1,JCL))
  999 END
