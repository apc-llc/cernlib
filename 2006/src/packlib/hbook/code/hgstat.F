*
* $Id: hgstat.F,v 1.1.1.1 1996/01/16 17:07:39 mclareni Exp $
*
* $Log: hgstat.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:39  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.43.42  by  Julian Bunn
*-- Author :
      SUBROUTINE HGSTAT(IDD,STAT)
*.==========>
*.       RETURN SUM OF CONTENTS OF IDD (WITH UNDER OVERFLOWS )
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      DIMENSION STAT(9)
*.___________________________________________
C             FIND HISTO WITH IDENTIFIER IDD AND MAKE IT THE
C             CURRENT HISTO
C
      CALL HFIND(IDD,'HGSTAT')
C
C             IDD IS AN INVALID IDENTIFIER, RETURN
C
      IF(LCID.EQ.0)GO TO 99
      NCX=IQ(LPRX)
      IF(LPRY.NE.0)GO TO 20
C
C             1-DIM HIST OR PROJECTION
C
C             UNDERFLOW
C
      STAT(1)=HCX(0,1)
C
C             IN RANGE
C
      STAT(2)=0.
      DO 10 I=1,NCX
   10 STAT(2)=STAT(2)+HCX(I,1)
C
C             OVERFLOW
C
      STAT(3)=HCX(NCX+1,1)
      STAT(4)=-1.0E11
      GO TO 99
C
C             2-DIM HIST
C
   20 NCY=IQ(LPRY)
C
C             STAT(1)= (X-UFLW, Y-OFLW)
C
      STAT(1)=HCXY(0,NCY+1,1)
      STAT(2)=0.
C
C             STAT(3)= (X-UFLW, Y-OFLW)
C
      STAT(3)=HCXY(NCX+1,NCY+1,1)
      STAT(4)=0.
      STAT(5)=0.
      STAT(6)=0.
C
C             STAT(4)= (X-UFLW, Y-RANGE)
C             STAT(5)= (X-RANGE, Y-RANGE)
C             STAT(6)= (X-OFLW, Y-RANGE)
C
      DO 40 I=1,NCY
         STAT(4)=STAT(4)+HCXY(0,I,1)
         DO 45 J=1,NCX
   45    STAT(5)=STAT(5)+HCXY(J,I,1)
   40 STAT(6)=STAT(6)+HCXY(NCX+1,I,1)
C
C             STAT(7)= (X-UFLW, Y-UFLW)
C
      STAT(7)=HCXY(0,0,1)
      STAT(8)=0.
C
C             STAT(2)= (X-RANGE, Y-OFLW)
C             STAT(8)= (X-RANGE, Y-UFLW)
C
      DO 30 I=1,NCX
         STAT(2)=STAT(2)+HCXY(I,NCY+1,1)
   30 STAT(8)=STAT(8)+HCXY(I,0,1)
C
C             STAT(9)= (X-OFLW, Y-UFLW)
C
      STAT(9)=HCXY(NCX+1,0,1)
C
   99 RETURN
      END
