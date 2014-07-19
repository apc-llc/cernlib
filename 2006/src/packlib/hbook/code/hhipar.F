*
* $Id: hhipar.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hhipar.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.44.08  by  Julian Bunn
*-- Author :
      SUBROUTINE HHIPAR(IFLRET)
*.==========>
*.           PUTS HISTOGRAM PARAMETERS
*.           NUMEP,ALLCHA,BINWID,XMIN,WGTMAX,EPSW ,YMIN,BINWIY IF ANY,
*.                           NCHANX INTO /HCFIT2/
*.                            RETURNS IFLRET=0 IF HISTOGRAM IS EMPTY
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcfitr.inc"
*.___________________________________________
      IFLRET=0
*
      EPSW=1.E-10
      BINWID=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
      IF(IFTRNG.NE.0)THEN
         ICX1=IFXLOW
         IF(IFXUP.GT.IQ(LPRX))IFXUP=IQ(LPRX)
         ICX2=IFXUP
         NCHANX=ICX2-ICX1+1
         XMIN=Q(LPRX+1)+(ICX1-1)*BINWID
      ELSE
         NCHANX=IQ(LPRX)
         ICX1=1
         ICX2=NCHANX
         XMIN=Q(LPRX+1)
      ENDIF
      NCHANY=1
      ICY1=1
      ICY2=1
      IF(IFLSF.NE.0)GO TO 98
      IF(IDIMPN.NE.3)THEN
         BINWIY=(Q(LPRY+2)-Q(LPRY+1))/FLOAT(IQ(LPRY))
         IF(IFTRNG.NE.0)THEN
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
*             COMPUTES ALLCHA AND WGTMAX(MAX. CONTENTS)
*
      ALLCHA=0.
      WGTMAX=0.
      DO 30 J=ICY1,ICY2
         DO 20 I=ICX1,ICX2
            IF(IDIMPN.EQ.3)CONTEN=HCX(I,1)
            IF(IDIMPN.EQ.4)CONTEN=HCXY(I,J,1)
            IF(CONTEN.LT.0.AND.LQ(LCONT).EQ.0)IWEIGH=1
            IF(ABS(CONTEN).GT.WGTMAX)WGTMAX=ABS(CONTEN)
            ALLCHA=ALLCHA+CONTEN
  20     CONTINUE
  30  CONTINUE
*
*             IF(HISTOGRAM IS EMPTY)RETURN
*
      IF(WGTMAX.EQ.0)THEN
         CALL HBUG('Empty histogram','HFIT**',ID)
         GO TO 99
      ENDIF
*
  98  IF(IFLSF.NE.0)NUMEP=NCHANX/IDIMPN
      IFLRET=1
*
  99  RETURN
      END
