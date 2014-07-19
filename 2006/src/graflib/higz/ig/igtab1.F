*
* $Id: igtab1.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtab1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.09/00 07/12/89  16.18.00  by  O.Couet
*-- Author :
      INTEGER FUNCTION IGTAB1(ELEV1,ICONT1,X1,Y1,ELEV2,ICONT2,X2,Y2
     +,                       XARR,YARR,ITARR,NCONT)
*.===========>
*.
*. Fill the matrix XARR YARR for Contour Plot
*.
*..==========> (O.Couet)
#include "higz/hicont.inc"
      DIMENSION XARR(*),YARR(*),ITARR(*)
      LOGICAL VERT
*.______________________________________
*
      IF(X1.EQ.X2)THEN
         VERT=.TRUE.
         TLEN=Y2-Y1
      ELSE
         VERT=.FALSE.
         TLEN=X2-X1
      ENDIF
*
      N=ICONT1+1
      TDIF=ELEV2-ELEV1
      I=1
      ICOUNT=0
   10 IF(N.GT.ICONT2.OR.I.GT.102)GOTO 20
      ELEV=N*DZ/NCONT +ZMIN
      DIFF=ELEV-ELEV1
      PDIF=DIFF/TDIF
      XLEN=TLEN*PDIF
      IF(VERT)THEN
         XARR(I)=X1
         YARR(I)=Y1+XLEN
      ELSE
         XARR(I)=X1+XLEN
         YARR(I)=Y1
      ENDIF
      ITARR(I)=N
      ICOUNT=ICOUNT+1
      I=I+2
      N=N+1
      GOTO 10
   20 IGTAB1=ICOUNT
*
      END
