*
* $Id: hexdat.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hexdat.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.40.09  by  Julian Bunn
*-- Author :
      SUBROUTINE HEXDAT(IEXDAT,IFLRET)
*.==========>
*.           PREPARES Q(IEXDAT)-ARRAY FOR HFUMIL AND RETURNS
*.           IFLRET=0,IF HISTOGRAM IS EMPTY,OTHERWISE IFLRET=1
*.           PUTS HISTOGRAM PARAMETRS NUMEP,XMIN,EPSW,ALLCHA,BINWID,
*.           WGTMAX INTO THE /HCFIT2/
*..=========> ( I.Ivanchenko )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfit2.inc"
*.___________________________________________
      IFLRET=0
      NCHANX=IQ(LPRX)
      NCHANY=1
      BINWID=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(NCHANX)
      CUR1X=Q(LPRX+1)-BINWID*0.5
*
*             PREPARATION EXDATA (F,SIGF,X),SIGF=SQRT(F)
*             IF(F.LT.0.)SIGF=1. IF((F/FMAX).LT.EPSW)EJECT BIN
*
      L=1
      EPSW=1.E-10
      ALLCHA=0.
*
      IF(IDIMPN.NE.3)THEN
         NCHANY=IQ(LPRY)
         BINWIY=(Q(LPRY+2)-Q(LPRY+1))/FLOAT(NCHANY)
         CURY=Q(LPRY+1)-BINWIY*0.5
      ENDIF
*
*             CHECK OUT OF ERRORBARS PRESENCE
      IF(LQ(LCONT).NE.0.AND.IDIMPN.EQ.3)GO TO 16
*
      WGTMAX=0.
      K=1
      DO 4 J=1,NCHANY
         DO 3 I=1,NCHANX
            LL1 = IEXDAT + IDIMPN*(K-1)
            IF( IDIMPN.EQ.3 )  Q(LL1) = HCX (I,1)
            IF( IDIMPN.EQ.4 )  Q(LL1) = HCXY(I,J,1)
            IF( ABS( Q(LL1) ).GT.WGTMAX )   WGTMAX = ABS( Q(LL1) )
            K=K+1
   3     CONTINUE
   4  CONTINUE
*
*             IF(HISTOGRAM IS EMPTY)RETURN
*
      IF(WGTMAX.EQ.0.)CALL HBUG('Empty histogram','HFIT**',ID)
      IF(WGTMAX.EQ.0.)GO TO 99
*
      K=1
      DO 14 J=1,NCHANY
         IF(IDIMPN.EQ.4)CURY=CURY+BINWIY
         CURX=CUR1X
         DO 13 I=1,NCHANX
            CURX=CURX+BINWID
            LL1 = IEXDAT + IDIMPN*(K-1)
            LL2 = IEXDAT + IDIMPN*(L-1)
            IF(IWEIGH.NE.0)THEN
               Q(LL2+1) = 1.
            ELSE
               IF( ABS( Q(LL1) )/WGTMAX .LE. EPSW )   GO TO 14
               Q(LL2) = Q(LL1)
               Q(LL2+1) = SQRT( ABS( Q(LL2) ) )
            ENDIF
            IF( IDIMPN.EQ.4 )   Q(LL2+3) = CURY
            Q(LL2+2) = CURX
            ALLCHA = ALLCHA + Q(LL2)
            L=L+1
            K=K+1
  13     CONTINUE
  14  CONTINUE
      GO TO 18
*
  16  CONTINUE
      CURX=CUR1X
      DO 17 K=1,NCHANX
         CURX=CURX+BINWID
         LL2 = IEXDAT + IDIMPN*(L-1)
         Q(LL2+1) = HCX(K,2)
         IF( Q(LL2+1).EQ.0 )   GO TO 17
         Q(LL2) = HCX(K,1)
         Q(LL2+2) = CURX
         ALLCHA = ALLCHA + Q(LL2)
         L=L+1
  17  CONTINUE
  18  NUMEP=L-1
*
*             IF(HISTOGRAM IS EMPTY)RETURN
*
      IF(NUMEP.EQ.0)CALL HBUG('Empty histogram','HFIT**',ID)
      IF(NUMEP.EQ.0)GO TO 99
      IFLRET=1
*
  99  RETURN
      END
