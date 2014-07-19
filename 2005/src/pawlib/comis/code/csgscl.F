*
* $Id: csgscl.F,v 1.1.1.1 1996/02/26 17:16:20 mclareni Exp $
*
* $Log: csgscl.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:20  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.26.35  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSGSCL(IGSCOM,IGSS,IGSP,IPAR)
***--------------------------------------------
*       a table-drived routine for switch to different stage of
*       comis compiler.
***--------------------------------------------
        COMMON /CSGSCM/ IGSST,JGSST,NGSST,NPAR,JSB,LABEL
        COMMON /MDPOOL/ IQ(6)
        DIMENSION IGSCOM(1),IGSS(1),IGSP(1),IPAR(1)
        DATA ISTK/0/
C
      J=MJCHAR(IGSS(1))
      IF(ISTK.EQ.0) GO TO 1
      IF(IQ(ISTK+1).EQ.J) GO TO 65
C  NEW MODULE:
    1 I=MHLOC(8)
      IQ(I)=ISTK
      ISTK=I
      IQ(I+1)=J
      IQ(I+6)=NPAR
      IQ(I+7)=JSB
      ISNXT=1
      ISN=2
      ISCUR=IGSP(1)
      IF(ISCUR.NE.0) GO TO 3
      I=1
    2 IGSP(I)=ISCUR+J
      I=I+2
      ISCUR=IGSP(I)
      IF(ISCUR.GE.0) GO TO 2
C  TOP OF MODULE STACK:
    3 ICUR=MHLOC(NPAR+5)
      IQ(ISTK+2)=ICUR
      IQ(ICUR)=0
      IQ(ICUR+3)=ISN
      IQ(ICUR+4)=ISNXT
*    4 IQ(ICUR+1)=JGSST
*      IQ(ICUR+2)=NGSST
*      CALL CCOPYA(IPAR(1),IQ(ICUR+5),NPAR)
   4  CONTINUE
C  NEXT ELEMENT:
   10 ISCUR=ISN
      IF(IGSCOM(ISCUR).NE.0) GO TO 20
C  END OF GRAF STRING:
      ISN=IQ(ICUR+3)
      ISNXT=IQ(ICUR+4)
      I=IQ(ICUR)
      CALL MHFREE(ICUR)
      ICUR=I
      IF(ICUR.EQ.0) GO TO 100
      GO TO 10
C  ELEMENT PROCESSING:
   20 GO TO (21,22,30,40,50,60,70,71),IGSCOM(ISCUR)
C  T:
   21 IGSST=1
      ISN=ISCUR+1
      GO TO 10
C  F:
   22 IGSST=0
      ISN=ISCUR+1
C  "FALSE" PROCESSING:
   23 IF(IGSCOM(ISN).NE.5) GO TO 24
      ISN=ISN+2
      GO TO 10
C  FAIL OF GRAF STRING:
*   24 JGSST=IQ(ICUR+1)
*      N=IQ(ICUR+2)
*      IF(N.GT.0)NGSST=N
*      IF(JSB.NE.0) CALL CSBCLL(JSB,IPAR(1),IQ(ICUR+5),NPAR)
*      CALL CCOPYA(IQ(ICUR+5),IPAR(1),NPAR)
  24  ISNXT=IGSCOM(ISNXT)
      ISN=ISNXT+1
      IF(ISNXT.NE.0) GO TO 10
C  FAIL OF INTERNAL SYMBOL:
      ISN=IQ(ICUR+3)
      ISNXT=IQ(ICUR+4)
      I=IQ(ICUR)
      CALL MHFREE(ICUR)
      ICUR=I
      IF(ICUR.EQ.0) GO TO 100
      GO TO 23
C  E:
   30 IGSST=-IGSCOM(ISCUR+1)
C  ERROR:
   31 I=IQ(ISTK+2)+5
*      IF(JSB.NE.0) CALL CSBCLL(JSB,IPAR(1),IQ(I),NPAR)
*      CALL CCOPYA(IQ(I),IPAR(1),NPAR)
   32 IF(ICUR.EQ.0) GO TO 100
      I=IQ(ICUR)
      CALL MHFREE(ICUR)
      ICUR=I
      GO TO 32
C  'TEXT':
   40 I=IGSCOM(ISCUR+1)
      ISN=ISCUR+2
      IGSST=MCSGES(JGSST,NGSST,IGSP(I),IGSP(I+1))
      IF(IGSST.EQ.0) GO TO 23
      GO TO 10
C  IF(TRUE) GO:
   50 ISN=IGSCOM(ISCUR+1)
      GO TO 10
C  EXTERNAL SYMBOL:
   60 IQ(ISTK+3)=ICUR
      IQ(ISTK+4)=ISCUR+2
      IQ(ISTK+5)=ISNXT
      LABEL=IGSCOM(ISCUR+1)
      IGSST=1
      RETURN
C  ENTRY ON COMPLETION OF EXTERNAL SYMBOL:
   65 ICUR=IQ(ISTK+3)
      ISN=IQ(ISTK+4)
      ISNXT=IQ(ISTK+5)
*      NPAR=IQ(ISTK+6)
*      JSB=IQ(ISTK+7)
      IF(IGSST) 31,23,10
C  CALL OF INTERNAL SYMBOL:
   70 I=MHLOC(NPAR+5)
      IQ(I)=ICUR
      ICUR=I
      IQ(ICUR+3)=ISCUR+2
      IQ(ICUR+4)=ISNXT
C  GO TO STATE:
   71 ISNXT=IGSCOM(ISCUR+1)
      ISN=ISNXT+1
      GO TO 4
C  END OF GRAF:
  100 I=ISTK
      ISTK=IQ(I)
      CALL MHFREE(I)
      LABEL=1
      RETURN
      END
