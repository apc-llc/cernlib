*
* $Id: hfith1.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfith1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.41.22  by  Julian Bunn
*-- Author :
      SUBROUTINE HFITH1(EXDA,ICELL)
*.==========>
*.           Gets histogram bin/cell content into EXDA
*..=========> ( I.Ivanchenko )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcfitr.inc"
      DIMENSION EXDA(4)
*.___________________________________________
*
      ICHANX=MOD(ICELL,NCHANX)
      IF(ICHANX.EQ.0)ICHANX=NCHANX
      RCHANX=ICHANX
      ICHX=ICHANX
      IF(IFTRNG.NE.0)THEN
         ICHANX=ICHANX+IFXLOW-1
      ENDIF
*
      IF(IDIMPN.EQ.3)THEN
         EXDA(1)=HCX(ICHANX,1)
         IF(I6.EQ.0)THEN
            EXDA(3)=XMIN+(RCHANX-0.5)*BINWID
         ELSE
            LBINS=LQ(LCID-2)
            EXDA(3)=0.5*(Q(LBINS+ICHANX)+Q(LBINS+ICHANX+1))
         ENDIF
      ELSE
         ICHANY=(ICELL-ICHX)/NCHANX+1
         RCHANY=ICHANY
         IF(IFTRNG.NE.0)THEN
            ICHANY=ICHANY+IFYLOW-1
         ENDIF
         EXDA(1)=HCXY(ICHANX,ICHANY,1)
         EXDA(3)=XMIN+(RCHANX-0.5)*BINWID
         EXDA(4)=YMIN+(RCHANY-0.5)*BINWIY
      ENDIF
*
*     Check if there are error bars
*
      IF(IWEIGH.NE.0)THEN
         IF(IWEIGH.EQ.1)THEN
            IF(EXDA(1).NE.0.)THEN
               EXDA(2)=1.
            ELSE
               EXDA(2)=0.
            ENDIF
         ELSEIF(IWEIGH.EQ.2)THEN
            EXDA(2)=1.
         ELSE
            EXDA(2)=WGTMAX
         ENDIF
         GO TO 99
      ENDIF
*
      IF(LQ(LCONT).EQ.0)THEN
         EXDA(2)=SQRT(ABS(EXDA(1)))
      ELSE
         IF(IDIMPN.EQ.3)THEN
            EXDA(2)=HCX(ICHANX,2)
         ELSE
            LW=LQ(LCONT)
            EXDA(2)=SQRT(ABS(Q(LW+ICELL)))
         ENDIF
      ENDIF
      IF(EXDA(2).EQ.0.)THEN
         EXDA(2)=ABS(EXDA(1))
         IF(EXDA(2).EQ.0.)GO TO 99
      ENDIF
*
   99 RETURN
      END
