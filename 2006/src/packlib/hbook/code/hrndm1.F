*
* $Id: hrndm1.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrndm1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      FUNCTION HRNDM1(IDD)
*.==========>
*.           RETURN IN HRNDM1 A RANDOM NUMBER DISTRIBUTED
*.           FOLLOWING THE CONTENTS OF HISTOGRAM IDD.
*.                  ( IDD CAN CONTAINS A FUNCTION  )
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      SAVE JDLAST,NER,NCHA,XMIN,XWID
      DATA JDLAST/0/
      DATA NER/0/
*.___________________________________________
      IF(IDD.NE.IDLAST)GO TO 5
      IF(IDD.EQ.JDLAST)GO TO 40
*
*             LOCATE IDD IN BLANK COMMON
*
   5  JDLAST=IDD
      ID=IDD
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)GO TO 99
      LCID=LQ(LTAB-IDPOS)
      LCONT=LQ(LCID-1)
      IDLAST=IDD
*
      NCHA=IQ(LCID+KNCX)
      XMIN=Q(LCID+KXMIN)
      XWID=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
*
*             COMPUTE INTEGRALE AND NORMALIZE
      IF(IQ(LCONT+KNOENT).EQ.-1)GO TO 40
      IF(IQ(LCONT+KNBIT).GE.32)GO TO 7
      IF(NER.EQ.0)CALL HBUG('Histogram with packing','HRNDM1',IDD)
      NER=1
      GO TO 99
*
  7   IQ(LCONT+KNOENT)=-1
  10  CONTINUE
*
      DO 20 I=2,NCHA
  20  Q(LCONT+KCON1+I)=Q(LCONT+KCON1+I)+Q(LCONT+KCON1+I-1)
      IF(Q(LCONT+KCON1+NCHA).NE.0.)GO TO 27
*
      DO 25 I=1,NCHA
  25  Q(LCONT+KCON1+I)=1.
      CALL HBUG('Integral is zero','HRNDM1',IDD)
      GO TO 10
*
  27  CONTINUE
      DO 30 I=1,NCHA
  30  Q(LCONT+KCON1+I)=Q(LCONT+KCON1+I)/Q(LCONT+KCON1+NCHA)
      Q(LCONT+KCON1)=0.
      CALL SBIT0(IQ(LCID),5)
*
*
*             NORMAL ENTRY
*
  40  R=RNDM(XMIN)
      ICHA=LOCATF(Q(LCONT+KCON1+1),NCHA,R)
      KCHA=IABS(ICHA)
      ICHA=KCHA+LCONT+KCON1+1
      DY=Q(ICHA)-Q(ICHA-1)
      IF(DY.GT.0.)THEN
         HRNDM1=XMIN+(R-Q(ICHA-1))*XWID/DY + XWID*FLOAT(KCHA)
      ELSE
         HRNDM1=XMIN + XWID*FLOAT(KCHA)
      ENDIF
      RETURN
*
  99  HRNDM1=0.
      IDLAST=0
      END
