*
* $Id: mnbias.F,v 1.1.1.1 1996/01/11 14:14:40 mclareni Exp $
*
* $Log: mnbias.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:40  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE MNBIAS
C     *****************
C-- GENERATES SPECTATOR JETS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/jet.inc"
#include "cojets/keybre.inc"
#include "cojets/maxn.inc"
#include "cojets/mb.inc"
#include "cojets/par.inc"
#include "cojets/parqua.inc"
#include "cojets/pmbias.inc"
#include "cojets/qcds.inc"
      DIMENSION EPPR(2),EMPR(2),PMB(4),V1(4),V2(4),EPPRX(2)
      INTEGER NLEAD(2)
C
      DATA ICALL/0/
C
C-- INITIALIZATION
      IF(ICALL.GT.0) GO TO 5
      ICALL=1
      CALL SBLOCK
      C=CLPRO
      CLEAD=C
      CINV=1./C
      EC=1.-EXP(-C)
      NLEAD(1)=102
      IF(IPBAR.NE.1) NLEAD(1)=101
      NLEAD(2)=101
5     CONTINUE
C
C-- PAIR OF LEADING NUCLEONS
      EPPRX(1)=PMBIAS(4)+PMBIAS(3)
      EPPRX(2)=PMBIAS(4)-PMBIAS(3)
      NIT=0
      NPARTH=NPART
      NH=NPARTH
8     CONTINUE
      NPART=NPARTH
      DO 10 I=1,2
      IF(NPART+1.GT.MXPART) GO TO 500
      NPART=NPART+1
9     CONTINUE
      NIT=NIT+1
      IF(NIT.GT.30) GO TO 400
      EPPR(I)=EPPRX(I)*(-LOG(1.-EC*CJRN(I))*CINV)
      PT2=-PT2PRO*LOG(CJRN(I+1))
      EMPR(I)=(PMAS(100+I)**2+PT2)/EPPR(I)
      IF(EMPR(I).GT.EPPR(I)) GO TO 9
      PT=SQRT(PT2)
      PHIP=PI2*CJRN(I+2)
      PARHAD(NPART,1)=PT*COS(PHIP)
      PARHAD(NPART,2)=PT*SIN(PHIP)
      PARHAD(NPART,3)=(-1.)**(I+1)*(EPPR(I)-EMPR(I))*.5
      PARHAD(NPART,4)=(EPPR(I)+EMPR(I))*.5
      PARHAD(NPART,5)=PMAS(NLEAD(I))
      PARHAD(NPART,6)=NLEAD(I)
      PARHAD(NPART,7)=0
C-- EXTRA PARTICLE INFO
      IORIG(NPART)=0
      IDENT(NPART)=IDENTF(INT(PARHAD(NPART,6)))
      IDCAY(NPART)=0
10    CONTINUE
C-- DETERMINE 4-MOMENTUM OF SOFT HADRON SYSTEM
      NPARTL=NPART+1
      DO 11 J=1,4
11    PMB(J)=PMBIAS(J)-(PARHAD(NPART,J)+PARHAD(NPART-1,J))
      IF(PMB(4).LT.0.) GO TO 8
      W2=PMB(4)**2-PMB(1)**2-PMB(2)**2-PMB(3)**2
      IF(W2.LT.1.) GO TO 8
C-- CALL KNO TO GENERATE SOFT HADRON SYSTEM
      CALL KNO(SQRT(W2),PMB)
C-- LONG LORENTZ BOOST TO LAB SYSTEM
      IF(NPART.LT.NPARTL) RETURN
      PMB(1)=0.
      PMB(2)=0.
      NEW=1
      DO 110 I=NPARTL,NPART
      DO 111 J=1,4
111   V1(J)=PARHAD(I,J)
      CALL CJLORN(PMB,V1,V2,NEW)
      NEW=0
      DO 112 J=1,4
112   PARHAD(I,J)=V2(J)
113   PARHAD(I,7)=0
110   CONTINUE
      RETURN
400   WEIGHT=0.
      RETURN
C-- ABNORMAL EXIT
500   WRITE(ITLIS,501) MXPART,NEVENT,NQUA
501   FORMAT(5(/),1X,45HNO. OF PARTICLES DURING MNBIAS CALL EXCEEDS  ,
     1I10//1X,11HEVENT NO. =   ,I10,10X,10HNO. JETS =   ,I10
     3//1X,'INCREASE MXPART'
     4 //1X,'EXECUTION TERMINATED'
     4)
      CALL OVERDM
      RETURN
      END
