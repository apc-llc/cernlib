*
* $Id: hhxye.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hhxye.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.47.28  by  Julian Bunn
*-- Author :
      SUBROUTINE HHXYE (X,Y,EY)
*.==========>
*.           COPIES HISTOGRAM BINS (CELLS) ALONG WITH
*.           CONTENTS AND ERRORS INTO X, Y AND EY
*..=========> ( D.Lienart )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcpar1.inc"
#include "hbook/hcpout.inc"
      DIMENSION X(NPMAX,ND),Y(1),EY(1)
*
      EPSW=1.E-10
      BINWID=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
      CUR1X=Q(LPRX+1)-BINWID*0.499999
      NCHANX=IQ(LPRX)
      NCHANY=1
      IF (ND.EQ.2) THEN
         NCHANY=IQ(LPRY)
         BINWIY=(Q(LPRY+2)-Q(LPRY+1))/FLOAT(IQ(LPRY))
         CURY=Q(LPRY+1)-BINWIY*0.499999
      ENDIF
*
*             FILL (Y,EY,X), EY=SQRT(Y)
*             IF (Y.LT.0.) EY=1. -  IF (Y/YMAX.LT.EPSW) EJECT BIN
*
*             CHECK OUT OF ERRORBARS PRESENCE
*
      IF (LQ(LCONT).NE.0.AND.ND.EQ.1) THEN
         L=1
         CURX=CUR1X
         DO 5 K=1,NCHANX
            CURX=CURX+BINWID
            EY(L)=HCX(K,2)
            IF (EY(L).NE.0.) THEN
               Y(L)=HCX(K,1)
               X(L,1)=CURX
               L=L+1
            ENDIF
   5     CONTINUE
      ELSE
         YMAX=0.
         K=1
         DO 10 J=1,NCHANY
            DO 10 I=1,NCHANX
               IF (ND.EQ.1)  Y(K)=HCX(I,1)
               IF (ND.EQ.2)  Y(K)=HCXY(I,J,1)
               IF (ABS(Y(K)).GT.YMAX)  YMAX=ABS(Y(K))
               K=K+1
  10     CONTINUE
         IF (YMAX.EQ.0.) THEN
            CALL HBUG('Empty histogram','HHXYE',ID)
            IFLAG=6
            RETURN
         ENDIF
*
         K=1
         L=1
         DO 20 J=1,NCHANY
            IF (ND.EQ.2) CURY=CURY+BINWIY
            CURX=CUR1X
            DO 20 I=1,NCHANX
               CURX=CURX+BINWID
               IF (IOPT(3).EQ.1) THEN
                  EY(L)=1.
               ELSE
                  IF (ABS(Y(K))/YMAX.LE.EPSW)  GOTO 15
                  Y(L)=Y(K)
                  EY(L)=SQRT(ABS(Y(L)))
               ENDIF
               IF (ND.EQ.2) X(L,2)=CURY
               X(L,1)=CURX
               L=L+1
   15          K=K+1
   20    CONTINUE
      ENDIF
*
      NP=L-1
      IF (NP.EQ.0) THEN
         CALL HBUG('Empty histogram','HHXYE',ID)
         IFLAG=6
      ENDIF
      END
