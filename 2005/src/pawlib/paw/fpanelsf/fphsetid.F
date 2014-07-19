*
* $Id: fphsetid.F,v 1.2 1996/05/31 16:06:53 couet Exp $
*
* $Log: fphsetid.F,v $
* Revision 1.2  1996/05/31 16:06:53  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 19/04/95  15.08.38  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPHSETID(CHHID,IERFLG)
      CHARACTER*(*) CHHID
      INTEGER IERFLG
*.===========> Author: G. Kozlovsky, 1994
*. Set current histogram in PAW
*.
*. This code is copied form: PAFITH, HFITH
*.
*..==========>
*
#include "hbook/hcbook.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcbits.inc"
#include "paw/pawcom.inc"
#include "paw/pcrang.inc"
*
      COMMON/QUEST/IQUEST(100)
      DIMENSION IOPT(13)
      EQUIVALENCE (IOPTQ,IOPT(1)),(IOPTV,IOPT(2))
      EQUIVALENCE (IOPTB,IOPT(3)),(IOPTL,IOPT(4))
      EQUIVALENCE (IOPTD,IOPT(5)),(IOPTW,IOPT(6))
      EQUIVALENCE (IOPTR,IOPT(7)),(IOPTN,IOPT(8))
      EQUIVALENCE (IOPTT,IOPT(9)),(IOPTE,IOPT(10))
      EQUIVALENCE (IOPTM,IOPT(11)),(IOPTU,IOPT(12))
      EQUIVALENCE (IOPTF,IOPT(13))
C.
C.    ------------------------------------------------------------------
C.
         IERFLG = 0
         CALL HGETID(CHHID)
         IF(LCID.LE.0) IERFLG = 1

* Set NUMEP required by HFCNH (borrowed from HFITH)
***** this is wrong, but there from I get IOPTR and NDIM
      IF (ICRANG .EQ. 0) IOPTR = 0
*************************************************
      CALL HFIND(ID,'HFITH ')
      IF(LCID.EQ.0) RETURN
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         CALL HBUG('Empty histogram','HFITH',ID)
         RETURN
      ENDIF
      CALL HDCOFL
*
      NDIM=1
      IF(I1.EQ.0)THEN
         IF(LCONT.EQ.LQ(LCID-1)) NDIM=2
      ENDIF
*
      IDIMPN=2+NDIM
      IFLSF = 0
      ITFUM = 0
*
      BINWID=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
      IF(IOPTR.NE.0)THEN
         IFTRNG=1
         IFXLOW=IQUEST(11)
         IFXUP =IQUEST(12)
         ICX1=IFXLOW
         IF(IFXUP.GT.IQ(LPRX))IFXUP=IQ(LPRX)
         ICX2=IFXUP
         NCHANX=ICX2-ICX1+1
         XMIN=Q(LPRX+1)+(ICX1-1)*BINWID
      ELSE
         IFTRNG=0
         NCHANX=IQ(LPRX)
         ICX1=1
         ICX2=NCHANX
         XMIN=Q(LPRX+1)
      ENDIF
      NCHANY=1
      ICY1=1
      ICY2=1
      IF(IDIMPN.NE.3)THEN
         BINWIY=(Q(LPRY+2)-Q(LPRY+1))/FLOAT(IQ(LPRY))
         IF(IOPTR.NE.0)THEN
            IFYLOW=IQUEST(13)
            IFYUP =IQUEST(14)
            ICY1=IFYLOW
            IF(IFYUP.GT.IQ(LPRY))IFYUP=IQ(LPRY)
            ICY2=IFYUP
            NCHANY=ICY2-ICY1+1
            YMIN=Q(LPRY+1)+(ICY1-1)*BINWIY
         ELSE
            NCHANY=IQ(LPRY)
            ICY1=1
            ICY2=NCHANY
            YMIN=Q(LPRY+1)
         ENDIF
      ENDIF
      NUMEP=NCHANX*NCHANY
*
*
*             Computes ALLCHA and WGTMAX
*
      ALLCHA=0.
      WGTMAX=0.
      DO 20 J=ICY1,ICY2
         DO 10 I=ICX1,ICX2
            IF(IDIMPN.EQ.3)CONTEN=HCX(I,1)
            IF(IDIMPN.EQ.4)CONTEN=HCXY(I,J,1)
            IF(CONTEN.LT.0.AND.LQ(LCONT).EQ.0)IWEIGH=1
            IF(ABS(CONTEN).GT.WGTMAX)WGTMAX=ABS(CONTEN)
            ALLCHA=ALLCHA+CONTEN
   10    CONTINUE
   20 CONTINUE
*
*             Create bank for function values
*
      IF(IDIMPN.EQ.3)THEN
         CALL HSUPIS(0,0,ICX1,ICX2)
      ENDIF
*

      END
