*
* $Id: bjets.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bjets.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BJETS
C     ****************
C-- HANDLES NON-LEPTONIC DECAY MODES OF WEAK BOSON
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/boflag.inc"
#include "cojets/boson.inc"
#include "cojets/ctopdc.inc"
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/iflghv.inc"
#include "cojets/isjetn.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/jetnpc.inc"
#include "cojets/jetset.inc"
#include "cojets/njsetb.inc"
#include "cojets/par.inc"
#include "cojets/parqua.inc"
#include "cojets/qcds.inc"
#include "cojets/quaor.inc"
#include "cojets/quaor2.inc"
#include "cojets/rotq.inc"
      INTEGER IFLJET(2)
      DIMENSION SETIN(6)
C
      IFBOS=PBOS(6)
C-- Q.N.
      IF(IFBOS.NE.4) THEN
        IFLJET(1)=KDP(ICHDB,1)/1000
        IFLJET(2)=KDP(ICHDB,2)/1000
      ELSE
        IFLJET(1)=-KDP(ICHDB,1)/1000
        IFLJET(2)=-KDP(ICHDB,2)/1000
      ENDIF
C
C-- INITIALIZATION FOR JET GENERATION (QUANTA)
      NQUAB=NQUA
      NJSETB=NJSET
30    CONTINUE
      NQUA=NQUAB
      NJSET=NJSETB
      QSQW=PBOS(5)**2*.25
      SETIN(1)=0.
      SETIN(2)=0.
      W2=PBOS(5)**2
31    CONTINUE
      CALL PSQGEN(ABS(IFLJET(1)),QSQW,QSQ1,IGO)
      CALL PSQGEN(ABS(IFLJET(2)),QSQW,QSQ2,IGO)
      Q1=SQRT(QSQ1)
      Q2=SQRT(QSQ2)
      IF(Q1+Q2.GE.PBOS(5)) GO TO 31
      PCM2=(W2-(Q1+Q2)**2)*(W2-(Q1-Q2)**2)/(4.*W2)
      PCM=SQRT(PCM2)
      E1=SQRT(QSQ1+PCM2)
      E2=SQRT(QSQ2+PCM2)
      CPH=1.
      SPH=0.
C
      PCMV(1)=PCM
      AMV(1,1)=Q1
      AMV(2,1)=Q2
      IFLFV(1,1)=IFLJET(1)
      IFLFV(2,1)=IFLJET(2)
      PFLABV(1,1,1)=0.
      PFLABV(2,1,1)=0.
      PFLABV(3,1,1)=PCM
      PFLABV(4,1,1)=E1
      PFLABV(1,2,1)=0.
      PFLABV(2,2,1)=0.
      PFLABV(3,2,1)=-PCM
      PFLABV(4,2,1)=E2
C
C-- 1ST JET (QUANTA)
      IROT=1
      SETIN(3)=E1+PCM
      SETIN(5)=QSQ1
      SETIN(6)=IFLJET(1)
      IQUAOR=IQUAO1
      JETN=IQUAOR
      NJSETL=NJSET
      CALL JETQCD(SETIN)
      NQUA1=NQUA
      IF(IBOFLA.EQ.1)
     * JORIG(NJSETL+1)=-(JPACK*JETN+NTOPD(1))
C
C-- 2ND JET (QUANTA)
      IROT=3
      CT=-1.
      ST=0.
      SETIN(3)=E2+PCM
      SETIN(5)=QSQ2
      SETIN(6)=IFLJET(2)
      IQUAOR=IQUAO2
      JETN=IQUAOR
      NJSETL=NJSET
      CALL JETQCD(SETIN)
      IF(IBOFLA.EQ.1)
     * JORIG(NJSETL+1)=-(JPACK*JETN+NTOPD(2))
C
C-- HADRONIZATION
      NIT=0
40    NIT=NIT+1
      IF(NIT.GT.5) GO TO 30
      NQUAC=NQUA
      IF(IBOFLA.EQ.0) THEN
        NSTRT=1
        NPART=0
      ELSE
        NSTRT=NPART+1
      ENDIF
      CALL HADRON(NQUAB+1,NQUAC,0)
      IF(IFLGHV.EQ.1) RETURN
      NP1=0
      NP=0
      DO 42 I=NSTRT,NPART
      NP=NP+1
42    IF(INT(PARHAD(I,7)).LE.NQUA1) NP1=NP1+1
C
C-- CORRECT FOR EXACT BOSON MASS
      IK=0
      DO 51 I=NSTRT,NPART
      IK=IK+1
      K(IK,2)=PARHAD(I,6)
      DO 51 J=1,5
51    P(IK,J)=PARHAD(I,J)
      CALL BMCORR(PBOS(5),NP1,NP,IFLAG)
C
      NPART=NSTRT-1
      IF(IFLAG.EQ.1) GO TO 30
C
      RETURN
      END
