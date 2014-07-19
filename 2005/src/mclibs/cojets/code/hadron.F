*
* $Id: hadron.F,v 1.1.1.1 1996/01/11 14:14:38 mclareni Exp $
*
* $Log: hadron.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:38  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE HADRON(NQUA1,NQUA2,IMBIAS)
C     *************************************
C-- HANDLES HADRONIZATION OF FINAL STATE
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cutoff.inc"
#include "cojets/event.inc"
#include "cojets/forcsl.inc"
#include "cojets/fstate.inc"
#include "cojets/iflghv.inc"
#include "cojets/itapes.inc"
#include "cojets/jetset.inc"
#include "cojets/maxn.inc"
#include "cojets/nflav.inc"
#include "cojets/parqua.inc"
#include "cojets/pmbias.inc"
#include "cojets/qcd.inc"
#include "cojets/tabpsq.inc"
C
#include "cojets/data1.inc"
#include "cojets/decpar.inc"
#include "cojets/edpar.inc"
#include "cojets/jet.inc"
#include "cojets/keybre.inc"
#include "cojets/par.inc"
C
      DIMENSION IFRSLQ(2)
C
      DATA ICALL/0/
C
C-- PRELIMINARIES
      IF(ICALL.GT.0) GO TO 11
      ICALL=1
      CALL SBLOCK
      EPS=1.E-7
C
11    CONTINUE
      NPARTL=NPART
      NIT=0
15    CONTINUE
      NIT=NIT+1
      IF(NIT.GT.10) GO TO 122
      NPART=NPARTL
      IF(NQUA1.GT.NQUA2) GO TO 125
C
C-- FORCE SL
      IFLGHV=0
      IFRSLQ(1)=0
      IFRSLQ(2)=0
      IF(LFORSL.NE.0.AND.KFORSL.NE.6) THEN
        IFORX=0
        DO 301 L=1,NFORSL
        PTQ2X=0.
        DO 300 I=NQUA1,NQUA2
        IF(INT(ABS(PARQUA(I,6))).NE.KFORSL) GO TO 300
        IF(I.EQ.IFORX) GO TO 300
        PTQ2=PARQUA(I,1)**2+PARQUA(I,2)**2
        IFRSLQ(L)=I
        PTQ2X=PTQ2
300     CONTINUE
        IFORX=IFRSLQ(L)
301     CONTINUE
      ENDIF
C
      DO 100 I=NQUA1,NQUA2
C
C-- FORCE SL
      IFORSL=0
      IF(I.EQ.IFRSLQ(1).OR.I.EQ.IFRSLQ(2)) IFORSL=1
C-- PREPARE TO ENTER JETGEN
      PP=SQRT(PARQUA(I,1)**2+PARQUA(I,2)**2+PARQUA(I,3)**2)
      E=PARQUA(I,4)
      EBEG=E
      THETA=0.
      PHI=0.
      IFLBEG=PARQUA(I,6)
      CALL JETGEN(N)
      IF(IFORSL.EQ.-1) THEN
        IFLGHV=1
        NHVREJ=NHVREJ+1
        RETURN
      ENDIF
C
C-- APPROPRIATE ROTATION IS APPLIED
      PCOS=PARQUA(I,3)/PP
      IF(ABS(PCOS).LT.1.) THEN
        THETA=ACOSX(PCOS)
      ELSEIF(PCOS.GE.1.) THEN
        THETA=0.
      ELSEIF(PCOS.LE.-1.) THEN
        THETA=PI
      ENDIF
      IF((PARQUA(I,1)**2+PARQUA(I,2)**2).LT.EPS) GO TO 21
      PARQU1=PARQUA(I,1)
      PARQU2=PARQUA(I,2)
      PHI=ATAN2X(PARQU2,PARQU1)
      GO TO 22
21    PHI=0.
22    CONTINUE
      IF(PHI.LT.0.) PHI=PHI+2.*PI
      CALL EDITP(N)
C
C-- BOOKING
      IF(N.EQ.0) GO TO 100
      IF(NPART+N.GT.MXPART) GO TO 500
      JETISA=ABS(JORIG(JETQUA(I)))/JPACK
      DO 41 IH=1,N
      IH1=NPART+IH
      DO 42 J=1,5
42    PARHAD(IH1,J)=P(IH,J)
      PARHAD(IH1,6)=K(IH,2)
      PARHAD(IH1,7)=I
C--  EXTRA PARTICLE INFO
      IF(K(IH,1).LT.0) THEN
        IORIG(IH1)=IPACK*JETISA+(-K(IH,1)+NPART)
      ELSE
        IORIG(IH1)=-(IPACK*JETISA+JETQUA(I))
      ENDIF
      IDENT(IH1)=IDENTF(INT(PARHAD(IH1,6)))
      IF(KDEC(IH,2).GE.KDEC(IH,1)) THEN
        IDCAY(IH1)=IPACK*(KDEC(IH,1)+NPART)+(KDEC(IH,2)+NPART)
      ELSE
        IDCAY(IH1)=0
      ENDIF
41    CONTINUE
      NPART=NPART+N
100   CONTINUE
      IF(IMBIAS.EQ.0) RETURN
C
C-- PREPARE TO CALL MNBIAS
125   CONTINUE
      DO 200 J=1,3
200   PMBIAS(J)=0.
      PMBIAS(4)=ECM
      PMBIAS(5)=PMBIAS(4)
      IF(NPART.EQ.0) GO TO 105
      DO 201 J=1,4
      DO 201 IH=1,NPART
201   IF(IDCAY(IH).EQ.0) PMBIAS(J)=PMBIAS(J)-PARHAD(IH,J)
      IF(PMBIAS(4).LT.0.) GO TO 15
      AM2=PMBIAS(4)**2-PMBIAS(1)**2-PMBIAS(2)**2-PMBIAS(3)**2
      IF(AM2.LT.0.) GO TO 15
      PMBIAS(5)=SQRT(AM2)
      IF(PMBIAS(5).LT.ECM*.01.OR.PMBIAS(5).LT.5.) GO TO 15
      GO TO 105
122   CONTINUE
      WEIGHT=0.
      GO TO 90
C
105   CONTINUE
C
      CALL MNBIAS
C
90    CONTINUE
      MNPART=MAX(MNPART,NPART)
C
      RETURN
C
C
C-- ABNORMAL EXIT
500   WRITE(ITLIS,501) MXPART,NEVENT,NQUA
501   FORMAT(5(/),1X,28HNUMBER OF PARTICLES EXCEEDS   ,I10
     1//1X,11HEVENT NO. =  ,I10 ,10X,10HNO. JETS =     ,I10
     2//1X,'(SUB. HADRON)'
     3//1X,'INCREASE MXPART'
     5)
      CALL OVERDM
      RETURN
      END
