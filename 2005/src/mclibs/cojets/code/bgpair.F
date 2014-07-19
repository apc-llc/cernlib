*
* $Id: bgpair.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bgpair.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE BGPAIR
C     *****************
C-- GENERATES PAIR OF INITIAL PARTONS - CALLED BY BGEVEN
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/botqrk.inc"
#include "cojets/bxgen.inc"
#include "cojets/entrev.inc"
#include "cojets/event.inc"
#include "cojets/itapes.inc"
#include "cojets/qcds.inc"
#include "cojets/weakon.inc"
      INTEGER IXV(2)
      DATA ICALL/0/
C-- INITIALIZATION
      IF(ICALL.GT.0) GO TO 9
      ICALL=1
      DO 8 IX=1,NBIN
      TBXGEN(IX)=0.
      DO 8 J=6,7
8     TBXGEN(IX)=TBXGEN(IX)+TABQRK(IX,J)
      TBXT=0.
      DO 7 IX=1,NBIN
7     TBXT=TBXT+TBXGEN(IX)
      DO 6 IX=1,NBIN
6     TBXAL(IX)=TBXGEN(IX)/TBXT
      CALL ALIAS(LBXAL,TBXAL,NBIN)
C
      DO 5 IX=2,NBIN
5     TBXGEN(IX)=TBXGEN(IX)+TBXGEN(IX-1)
C
      XST=XMIN
      ALST=LOG(XMIN)
      IXST=INT(XMIN/DBIN)+2
      ALST1=LOG(FLOAT(IXST-1)*DBIN)
      F2X=F2IN(XST,6)+F2IN(XST,7)
9     CONTINUE
C
10    CONTINUE
      CALL BGEXIN(XST,IXST,ALST,ALST1,XING(1),IXV(1),1)
      CALL BGEXIN(XST,IXST,ALST,ALST1,XING(2),IXV(2),1)
C
      NGEX=NGEX+1
C
C-- CHECK WHETHER SHAT ABOVE THRESHOLD
      SHAT=XING(1)*XING(2)*S
       IF(SHAT.LT.BIML2) GO TO 10
C
      DO 75 L=1,2
      CALL BGEXIN(XST,IXST,ALST,ALST1,XING(L),IXV(L),2)
      IXV(L)=MAX(IXV(L),2)
75    CONTINUE
      SHAT=XING(1)*XING(2)*S
      IF(SHAT.LT.BIML2) GO TO 10
C
      DO 72 L=1,2
73    PT2=-PT2INT*LOG(CJRN(L))
      XMING(L)=PT2/(XING(L)*S)
      IF(XMING(L).GT.XING(L)) GO TO 73
      PT=SQRT(PT2)
      PHI=PI2*CJRN(L+1)
      PXING(L)=PT*COS(PHI)
      PYING(L)=PT*SIN(PHI)
      IX=IXV(L)
      IF(CJRN(L+2).GT.TABQRK(IX,6)/(TABQRK(IX,6)+TABQRK(IX,7))) GO TO 74
      CALL BGEFLA(IX,IFLING(L),JOUT)
      IF(L.EQ.1.AND.IPBAR.EQ.1) IFLING(L)=-IFLING(L)
      GO TO 72
74    IFLING(L)=LGLUS
72    CONTINUE
C
      END
