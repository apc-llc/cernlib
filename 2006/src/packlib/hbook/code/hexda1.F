*
* $Id: hexda1.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hexda1.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.39.39  by  Julian Bunn
*-- Author :
      SUBROUTINE HEXDA1(EXDA,ICELL,IFLRET)
*.==========>
*.           PUTS ONE PORTION OF DATA F,SIGF,ARGUMENT
*.           INTO THE EXDA
*..=========> ( I.Ivanchenko )
#include "hbook/hcbook.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcfitr.inc"
      DIMENSION EXDA(4)
*.___________________________________________
      IFLRET=0
*
      IF(IFLSF.NE.0)THEN
         ICHANX=(ICELL-1)*IDIMPN
         DO 1 I=1,IDIMPN
   1     EXDA(I)=HCX(ICHANX+I,1)
         IF(EXDA(2).EQ.0.)THEN
            EXDA(2)=ABS(EXDA(1))
            IF(EXDA(2).EQ.0.)GO TO 99
         ENDIF
         IFLRET=1
         GO TO 99
      ENDIF
*
      ICHANX=MOD(ICELL,NCHANX)
      IF(ICHANX.EQ.0)ICHANX=NCHANX
      RCHANX=ICHANX
      ICHX=ICHANX
      IF(IFTRNG.NE.0)THEN
         ICHANX=ICHANX+IFXLOW-1
      ENDIF
*
      EXDA(3)=XMIN+(RCHANX-0.5)*BINWID
      IF(IDIMPN.EQ.3)THEN
         EXDA(1)=HCX(ICHANX,1)
      ELSE
         ICHANY=(ICELL-ICHX)/NCHANX+1
         RCHANY=ICHANY
         IF(IFTRNG.NE.0)THEN
            ICHANY=ICHANY+IFYLOW-1
         ENDIF
         EXDA(1)=HCXY(ICHANX,ICHANY,1)
         EXDA(4)=YMIN+(RCHANY-0.5)*BINWIY
      ENDIF
*
*     CHECK OUT OF ERRORBARS PRESENCE
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
         IFLRET=1
         GO TO 99
      ENDIF
*
      IF(LQ(LCONT).NE.0.AND.IDIMPN.EQ.3)THEN
         EXDA(2)=HCX(ICHANX,2)
      ELSE
         EXDA(2)=SQRT(ABS(EXDA(1)))
      ENDIF
      IF(EXDA(2).EQ.0.)THEN
         EXDA(2)=ABS(EXDA(1))
         IF(EXDA(2).EQ.0.)GO TO 99
      ENDIF
*
      IFLRET=1
*
  99  RETURN
      END
