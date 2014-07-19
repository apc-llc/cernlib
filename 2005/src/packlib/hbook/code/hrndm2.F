*
* $Id: hrndm2.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrndm2.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      SUBROUTINE HRNDM2(IDD,XR,YR)
*.==========>
*.           RETURN IN XR AND YR 2 RANDOM NUMBERS DISTRIBUTED
*.           FOLLOWING THE CONTENTS OF IDD
*.           XR AND YR ARE 2 VARIABLES CORRELATED
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      SAVE JDLAST,NER,N,NCX,NCY,XMIN,YMIN,XWID,YWID
      DATA JDLAST/0/
      DATA NER/0/
*.___________________________________________
      IF(IDD.NE.IDLAST)GO TO 5
      IF(IDD.EQ.JDLAST)GO TO 80
*
*             LOCATES IDD IN BLANK COMMON
*
   5  JDLAST=IDD
      ID=IDD
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)GO TO 99
      LCID=LQ(LTAB-IDPOS)
      LCONT=LQ(LCID-1)
      LSCAT=LCONT
      IDLAST=IDD
*
*             IF IS THE POINTER TO THE NUMBER OF ENTRIES
*
      NCX=IQ(LCID+KNCX)+2
      XMIN=Q(LCID+KXMIN)
      XWID=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
      NCY=IQ(LCID+KNCY)+2
      YMIN=Q(LCID+KYMIN)
      YWID=(Q(LCID+KYMAX)-Q(LCID+KYMIN))/FLOAT(IQ(LCID+KNCY))
*
*             COMPUTE INTEGRALE AND NORMALISE
*
      N=NCX*NCY
*
      IF(IQ(LCONT+KNOENT).EQ.-1)GO TO 80
      IF(IQ(LCONT+KNBIT).GE.32)GO TO 10
      IF(NER.EQ.0)CALL HBUG('Histogram with packing','HRNDM2',IDD)
      NER=1
      GO TO 99
*
*
  10  K=LCONT+KCON2-1
      Q(K)=0.
      DO 42 JJ=1,NCY
         J=NCY-JJ+1
         DO 40 I=1,NCX
            K=K+1
            IF(I.GT.1.AND.I.LT.NCX)GO TO 20
            Q(K)=0.
            GO TO 30
  20        IF(J.GT.1.AND.J.LT.NCY)GO TO 30
            Q(K)=0.
  30        Q(K)=Q(K)+Q(K-1)
  40     CONTINUE
  42  CONTINUE
*
      IF(Q(K).NE.0.)GO TO 70
      CALL HBUG('Integral is zero','HRNDM2',IDD)
*
      K=LCONT+KCON2-1
      DO 62 JJ=1,NCY
         J=NCY-JJ+1
         DO 60 I=1,NCX
            K=K+1
            Q(K)=1.
            IF(I.GT.1.AND.I.LT.NCX)GO TO 45
            Q(K)=0.
            GO TO 50
  45        IF(J.GT.1.AND.J.LT.NCY)GO TO 50
            Q(K)=0.
  50        Q(K)=Q(K)+Q(K-1)
  60     CONTINUE
  62  CONTINUE
*
  70  CONTINUE
*
      DO 75 I=1,N
  75  Q(LCONT+KCON2+I-1)=Q(LCONT+KCON2+I-1)/Q(K)
      IQ(LCONT+KNOENT)=-1
      CALL SBIT0(IQ(LCID),5)
*
*
*             NORMAL ENTRY
*
*
  80  R=RNDM(XMIN)
      ICHA=LOCATF(Q(LCONT+KCON2),N,R)
      JCHA=IABS(ICHA)+LCONT+KCON2
      DXINT=Q(JCHA)-Q(JCHA-1)
      IF(DXINT.GT.0.)THEN
         DX=XWID*(R-Q(JCHA-1))/DXINT
      ELSE
         DX=0.
      ENDIF
      DY=YWID*RNDM(YMIN)
      ICHA=JCHA-LCONT-KCON2+1
*
      I=MOD(ICHA,NCX)
      IF(I.EQ.0)I=NCX
      J=NCY-(ICHA-I)/NCX
*
      XR=XMIN+XWID*FLOAT(I-2)+DX
      YR=YMIN+YWID*FLOAT(J-2)+DY
      RETURN
*
  99  XR=0.
      YR=0.
      IDLAST=0
      END
