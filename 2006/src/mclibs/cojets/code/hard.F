*
* $Id: hard.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: hard.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE HARD(J1,J2,IFLFF1,IFLFF2,SHAT,T)
C     *******************************************
C-- HANDLES KINEMATICS AND QCD EVOLUTION OF FINAL HARD QUANTA
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cutoff.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/iflghv.inc"
#include "cojets/imlf.inc"
#include "cojets/inmat.inc"
#include "cojets/isjetn.inc"
#include "cojets/itapes.inc"
#include "cojets/nflav.inc"
#include "cojets/parq.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/thrfla.inc"
#include "cojets/transl.inc"
#include "cojets/twobod.inc"
      DIMENSION PCMV1(4),PCMV2(4),SETIN(6)
C
      IFLGHV=0
      SH=SHAT
      IFLAF1=IFLFF1
      IFLAF2=IFLFF2
      MFLAF1=ABS(IFLAF1)
      MFLAF2=ABS(IFLAF2)
      IF(MFLAF1.EQ.LGLUS) MFLAF1=LGLUT
      IF(MFLAF2.EQ.LGLUS) MFLAF2=LGLUT
      XQSQ=.75*QSQ
      ICOUNT=0
20    CONTINUE
      ICOUNT=ICOUNT+1
      IF(ICOUNT.GT.20) XQSQ=0.
      CALL PSQGEN(MFLAF1,XQSQ,QSQ1,IGO)
      CALL PSQGEN(MFLAF2,XQSQ,QSQ2,IGO)
      Q1=SQRT(QSQ1)
      Q2=SQRT(QSQ2)
      IF((Q1+Q2)**2.GE.SHAT.AND.ICOUNT.LE.20) GO TO 20
      LHEAV=0
      IF(MFLAF1.NE.LGLUT.AND.MFLAF1.GT.3) LHEAV=LHEAV+1
      IF(MFLAF2.NE.LGLUT.AND.MFLAF2.GT.3) LHEAV=LHEAV+1
      IF(LHEAV.EQ.0) GO TO 50
      IF(LHEAV.EQ.1) GO TO 51
      IF(LHEAV.EQ.2) GO TO 52
50    COSCM=T/(.5*SHAT)+1.
      TH=T
      GO TO 60
51    IF(MFLAF1.NE.LGLUT.AND.MFLAF1.GT.3) A2=AM2HEV(MFLAF1)
      IF(MFLAF2.NE.LGLUT.AND.MFLAF2.GT.3) A2=AM2HEV(MFLAF2)
      COSCM=T/(.5*(SHAT-A2))+1.
      TH=T
      GO TO 60
52    A2=AM2HEV(MFLAF1)
      B2=AM2HEV(MFLAF2)
      AB2=.5*(A2+B2)
      AM=AMHEV(MFLAF1)
      BM=AMHEV(MFLAF2)
      QCM2=(SHAT-(AM-BM)**2)*(SHAT-(AM+BM)**2)/(4.*SHAT)
      QCM=SQRT(ABS(QCM2))
      QICM=SQRT(SHAT)/2.
      EA=SQRT(A2+QCM2)
      TTRUE=T+AB2
      COSCM=(TTRUE-A2+2.*EA*QICM)/(2.*QCM*QICM)
      TH=TTRUE
60    CONTINUE
      PCM2=(SHAT-(Q1+Q2)**2)*(SHAT-(Q1-Q2)**2)/(4.*SHAT)
      PCM=SQRT(ABS(PCM2))
      PCMV1(4)=SQRT(PCM2+QSQ1)
      PCMV1(3)=PCM*COSCM
      SINCM=SQRT(ABS(1.-COSCM**2))
      PHI=PI2*CJRN(0)
      PCMV1(1)=PCM*SINCM*COS(PHI)
      PCMV1(2)=PCM*SINCM*SIN(PHI)
      DO 10 J=1,3
10    PCMV2(J)=-PCMV1(J)
      PCMV2(4)=SQRT(PCM2+QSQ2)
      NEW=1
      CALL LORLAB(PCMV1,PF1,NEW)
      CALL LORLAB(PCMV2,PF2,NEW)
      DO 30 J=1,4
30    PFTOT(J)=PF1(J)+PF2(J)
      IF(IFLAF1.EQ.LGLUS) IFLAF1=0
      IF(IFLAF2.EQ.LGLUS) IFLAF2=0
C
      JETN=1
      CALL INSET(PF1,IFLAF1,QSQ1,1,SETIN)
      CALL JETQCD(SETIN)
      JETN=2
      CALL INSET(PF2,IFLAF2,QSQ2,2,SETIN)
      CALL JETQCD(SETIN)
C
      IF(KPRHEV.LT.3) RETURN
      NQHEV=0
      DO 62 IQ=1,NQUA
      IF(INT(ABS(PARQUA(IQ,6))).NE.KFRFLA) GO TO 62
      NQHEV=NQHEV+1
62    CONTINUE
      IF(KPRHEV.EQ.3.AND.NQHEV.EQ.0) IFLGHV=1
      IF(KPRHEV.EQ.4) THEN
        IF(IM.GT.5) THEN
          IF(NQHEV.LE.2) IFLGHV=1
        ELSEIF((IM.EQ.3.OR.IM.EQ.5).AND.LF.GE.4) THEN
          IF(NQHEV.LE.2) IFLGHV=1
        ELSE
          IF(NQHEV.EQ.0) IFLGHV=1
        ENDIF
      ENDIF
      IF(IFLGHV.EQ.1) NHVREJ=NHVREJ+1
      RETURN
      END
