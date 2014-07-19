*
* $Id: hwhepa.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhepa.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHEPA.
*CMZ :-        -26/04/91  14.55.44  by  Federico Carminati
*-- Author :    Bryan Webber and Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWHEPA
C------------------------------------------------------------------------
C     (Initially polarised) e+e- --> ffbar (f=quark, mu or tau)
C     If IPROC=107: --> gg, distributed as sum of light quarks.
C     If fermion flavour specified mass effects fully included.
C     EVWGT=sig(e+e- --> ffbar) in nb
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER ID1,ID2,IDF,IQ,IQ1,I
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUPCM,HWUAEM, Q2NOW,Q2LST,FACTR,
     & VF2,VF,CLF(7),PRAN,PQWT,PMAX,PTHETA,SINTH2,CPHI,SPHI,C2PHI,S2PHI,
     & PPHI,SINTH,PCM,PP(5)
      SAVE Q2LST,FACTR,ID1,ID2,VF2,VF,CLF
      DATA Q2LST/0./
      IF (GENEV) THEN
        IF (ID2.EQ.0) THEN
C Choose quark flavour
          PRAN=TQWT*HWRGEN(0)
          PQWT=0.
          DO 10 IQ=1,MAXFL
          PQWT=PQWT+CLQ(1,IQ)
          IF (PQWT.GT.PRAN) GOTO 11
   10     CONTINUE
          IQ=MAXFL
   11     IQ1=MAPQ(IQ)
          DO 20 I=1,7
   20     CLF(I)=CLQ(I,IQ)
        ELSE
          IQ1=ID1
        ENDIF
C Label particles, assign outgoing particle masses
        IDHW(4)=200
        IDHEP(4)=23
        ISTHEP(4)=110
        IF (ID1.EQ.7) THEN
           IDHW(5)=13
           IDHW(6)=13
           IDHEP(5)=21
           IDHEP(6)=21
           PHEP(5,5)=RMASS(13)
           PHEP(5,6)=RMASS(13)
        ELSE
           IDHW(5)=IQ1
           IDHW(6)=IQ1+6
           IDHEP(5)=IDPDG(IQ1)
           IDHEP(6)=-IDHEP(5)
           PHEP(5,5)=RMASS(IQ1)
           PHEP(5,6)=RMASS(IQ1)
        ENDIF
        ISTHEP(5)=113
        ISTHEP(6)=114
        JMOHEP(1,4)=1
        JMOHEP(2,4)=2
        JMOHEP(1,5)=4
        JMOHEP(2,5)=6
        JMOHEP(1,6)=4
        JMOHEP(2,6)=5
        JDAHEP(1,4)=5
        JDAHEP(2,4)=6
        JDAHEP(1,5)=0
        JDAHEP(2,5)=6
        JDAHEP(1,6)=0
        JDAHEP(2,6)=5
        NHEP=6
C Generate polar and azimuthal angular distributions:
C  CLF(1)*(1+(VF*COSTH)**2)+CLF(2)*(1-VF**2)+CLF(3)*2.*VF*COSTH
C +(VF*SINTH)**2*(CLF(4)*COS(2*PHI-PHI1-PHI2)
C                +CLF(6)*SIN(2*PHI-PHI1-PHI2))
        PMAX=CLF(1)*(1.+VF2)+CLF(2)*(1.-VF2)+ABS(CLF(3))*2.*VF
  30    COSTH=HWRUNI(0,-ONE, ONE)
        PTHETA=CLF(1)*(1.+VF2*COSTH**2)+CLF(2)*(1.-VF2)
     &        +CLF(3)*2.*VF*COSTH
        IF (PTHETA.LT.PMAX*HWRGEN(1)) GOTO 30
        IF (IDHW(1).GT.IDHW(2)) COSTH=-COSTH
        SINTH2=1.-COSTH**2
        IF (TPOL) THEN
           PMAX=PTHETA+VF2*SINTH2*SQRT(CLF(4)**2+CLF(6)**2)
  40       CALL HWRAZM(ONE,CPHI,SPHI)
           C2PHI=2.*CPHI**2-1.
           S2PHI=2.*CPHI*SPHI
           PPHI=PTHETA+(CLF(4)*(C2PHI*COSS+S2PHI*SINS)
     &                 +CLF(6)*(S2PHI*COSS-C2PHI*SINS))*VF2*SINTH2
           IF (PPHI.LT.PMAX*HWRGEN(1)) GOTO 40
        ELSE
           CALL HWRAZM(ONE,CPHI,SPHI)
        ENDIF
C Construct final state 4-mommenta
        CALL HWVEQU(5,PHEP(1,3),PHEP(1,4))
        PCM=HWUPCM(PHEP(5,4),PHEP(5,5),PHEP(5,6))
C PP is momentum of track 5 in CoM (track 4) frame
        SINTH=SQRT(SINTH2)
        PP(5)=PHEP(5,5)
        PP(1)=PCM*SINTH*CPHI
        PP(2)=PCM*SINTH*SPHI
        PP(3)=PCM*COSTH
        PP(4)=SQRT(PCM**2+PP(5)**2)
        CALL HWULOB(PHEP(1,4),PP(1),PHEP(1,5))
        CALL HWVDIF(4,PHEP(1,4),PHEP(1,5),PHEP(1,6))
      ELSE
        EMSCA=PHEP(5,3)
        Q2NOW=EMSCA**2
        IF (Q2NOW.NE.Q2LST) THEN
C Calculate coefficients for cross-section
          EMSCA=PHEP(5,3)
          Q2LST=Q2NOW
          FACTR=PIFAC*GEV2NB*HWUAEM(Q2NOW)**2/Q2NOW
          ID1=MOD(IPROC,10)
          ID2=MOD(ID1,7)
          IF (ID2.EQ.0) THEN
             CALL HWUEEC(1)
             VF2=1.
             VF=1.
             EVWGT=FACTR*FLOAT(NCOLO)*TQWT*4./3.
          ELSE
             IF (IPROC.LT.150) THEN
                IDF=ID1
                FACTR=FACTR*FLOAT(NCOLO)
             ELSE
                ID1=2*ID1+119
                IDF=ID1-110
             ENDIF
             IF (EMSCA.LE.2.*RMASS(ID1)) CALL HWWARN('HWHEPA',101,*999)
             CALL HWUCFF(11,IDF,Q2NOW,CLF(1))
             VF2=1.-4.*RMASS(ID1)**2/Q2NOW
             VF=SQRT(VF2)
             EVWGT=FACTR*VF*(CLF(1)*(1.+VF2/3.)+CLF(2)*(1.-VF2))
          ENDIF
        ENDIF
      ENDIF
  999 END
