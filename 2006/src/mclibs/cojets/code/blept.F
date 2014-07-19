*
* $Id: blept.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: blept.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BLEPT
C     ****************
C--  HANDLES LEPTONIC DECAY MODES OF WEAK BOSON
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
#include "cojets/isjetn.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/jetnpc.inc"
#include "cojets/qcds.inc"
#include "cojets/parqua.inc"
C
      IFBOS=PBOS(6)
C-- Q.N.
      IDC=ICHDB
      DO 10 I=1,2
10    K(I,2)=KDP(IDC,I)
      IF(IFBOS.NE.4) GO TO 12
      ID1=IDENTF(K(1,2))
      ID2=IDENTF(K(2,2))
      K(1,2)=INTID(-ID1)
      K(2,2)=INTID(-ID2)
12    CONTINUE
C-- KINEMATICS
      W2=(PBOS(5))**2
      AM1=PMAS(K(1,2))
      AM2=PMAS(K(2,2))
      PP=.5*SQRT((W2-(AM1+AM2)**2)*(W2-(AM1-AM2)**2)/W2)
      E1=SQRT(AM1**2+PP**2)
      E2=SQRT(AM2**2+PP**2)
C
      IF(NPART+2.GT.MXPART) GO TO 500
      DO 15 I=1,2
      FSIGN=1.
      IF(I.EQ.2) FSIGN=-1.
      P(I,1)=0.
      P(I,2)=0.
      P(I,3)=PP*FSIGN
      P(I,5)=PMAS(K(I,2))
      IP=NPART+I
      IF(IBOFLA.EQ.0) THEN
        IORIG(IP)=IPACK*I
        IDENT(IP)=IDENTF(K(I,2))
        IDCAY(IP)=0
      ENDIF
15    CONTINUE
      P(1,4)=E1
      P(2,4)=E2
      ICHN=ICHDB-IDB(IFBOS)+1
C
      PCMV(1)=PP
      AMV(1,1)=P(1,5)
      AMV(2,1)=P(2,5)
      IFLFV(1,1)=K(1,2)
      IFLFV(2,1)=K(2,2)
      DO 16 I=1,2
      DO 16 J=1,4
16    PFLABV(J,I,1)=P(I,J)
C
      DO 51 I=1,2
      KDEC(I,1)=1
      KDEC(I,2)=0
51    K(I,1)=0
      NPRIMR=2
      IF(ICHN.EQ.3) GO TO 50
C
C-- DECAY PRODUCTS DO NOT INCLUDE TAU PARTICLES
      NP=2
C
      RETURN
C
C-- TAU DECAYS
50    CONTINUE
      I=2
      IPD=0
53    IPD=IPD+1
      KDEC(IPD,1)=I+1
      IF(WEIGHT.LT.1.E-30) RETURN
      IF(IDB(K(IPD,2)).GT.0) CALL DECAY(IPD,I)
      KDEC(IPD,2)=I
      IF(IPD.LT.I) GO TO 53
      NP=I
      IF(IBOFLA.NE.0) RETURN
      IF(I.EQ.2) RETURN
      IF(NPART+NP.GT.MXPART) GO TO 500
      DO 56 I=1,NP
      IP=NPART+I
      IF(I.LE.2) GO TO 57
      IORIG(IP)=IPACK*(ABS(IORIG(-K(I,1)+NPART))/IPACK)
     *+(-K(I,1)+NPART)
      IDENT(IP)=IDENTF(K(I,2))
57    CONTINUE
      IDCAY(IP)=0
      IF(KDEC(I,2).GE.KDEC(I,1))
     *IDCAY(IP)=IPACK*KDEC(I,1)+KDEC(I,2)
56    CONTINUE
C
      RETURN
C
C-- ABNORMAL EXIT
500   WRITE(ITLIS,501) MXPART,NEVENT,NQUA
501   FORMAT(5(/),1X,28HNUMBER OF PARTICLES EXCEEDS   ,I10
     1//1X,11HEVENT NO. =  ,I10 ,10X,10HNO. JETS =     ,I10
     3//1X,'INCREASE MXPART'
     4)
      CALL OVERDM
      RETURN
C
      END
