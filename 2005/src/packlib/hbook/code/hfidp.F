*
* $Id: hfidp.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfidp.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
      SUBROUTINE HFIDP(ID1,X,Y,W)
*.==========>
*.           FILLS PROJECTIONS OF A 2-DIM
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
*.___________________________________________
*
*             PROX
*
      LCONT=LQ(LCID-2)
      IF(LCONT.NE.0)THEN
         LPRX=LCID+KNCX
         CALL HF1(ID1,X,W)
      ENDIF
*
*             PROY
*
      LCONT=LQ(LCID-3)
      IF(LCONT.NE.0)THEN
         LPRX=LCID+KNCY
         CALL HF1(ID1,Y,W)
      ENDIF
*
*             SLIX
*
      LSLIX=LQ(LCID-4)
      IF(LSLIX.NE.0)THEN
         NSL=IQ(LSLIX-2)
         ISL=FLOAT(NSL)*(Y-Q(LCID+KYMIN))/(Q(LCID+KYMAX)
     +     -Q(LCID+KYMIN))+1
         IF(ISL.GT.0.AND.ISL.LE.NSL)THEN
            LCONT=LQ(LSLIX-ISL)
            LPRX=LCID+KNCX
            CALL HF1(ID1,X,W)
         ENDIF
      ENDIF
*
*             SLIY
*
      LSLIY=LQ(LCID-5)
      IF(LSLIY.NE.0)THEN
         NSL=IQ(LSLIY-2)
         ISL=FLOAT(NSL)*(X-Q(LCID+KXMIN))/(Q(LCID+KXMAX)
     +     -Q(LCID+KXMIN))+1
         IF(ISL.GT.0.AND.ISL.LE.NSL)THEN
            LCONT=LQ(LSLIY-ISL)
            LPRX=LCID+KNCY
            CALL HF1(ID1,Y,W)
         ENDIF
      ENDIF
*
*             BAND  X
*
      LBANX=LQ(LCID-6)
      IF(LBANX.NE.0)THEN
  10     IF(Y.GE.Q(LBANX+1).AND.Y.LT.Q(LBANX+2))THEN
            LCONT=LQ(LBANX-1)
            LPRX=LCID+KNCX
            CALL HF1(ID1,X,W)
         ENDIF
         LBANX=LQ(LBANX)
         IF(LBANX.NE.0)GO TO 10
      ENDIF
*
*             BAND  Y
*
      LBANY=LQ(LCID-7)
      IF(LBANY.NE.0)THEN
  20     IF(X.GE.Q(LBANY+1).AND.X.LT.Q(LBANY+2))THEN
            LCONT=LQ(LBANY-1)
            LPRX=LCID+KNCY
            CALL HF1(ID1,Y,W)
         ENDIF
         LBANY=LQ(LBANY)
         IF(LBANY.NE.0)GO TO 20
      ENDIF
*
      END
