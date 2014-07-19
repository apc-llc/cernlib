*
* $Id: hfact.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfact.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 22/05/89  16.23.19  by  Rene Brun
*-- Author :
      SUBROUTINE HFACT(A,N,ISIGNE,IEXP1,IEXP2,FACT)
*.==========>
*.           THIS ROUTINE CHANGES ARRAY A SUCH THAT MAXIMUM OF A
*.           IS .LT. 10000. OR .GE. 0.001  . FACT IS THE MULTIPLYING
*.               FACTOR
*.           EX.  IF FACT = 10**-3  ISIGNE=1H- ,IEXP1=1H ,IEXP2=1H3
*..=========> ( R.Brun )
      DIMENSION A(N),IB(2)
      SAVE IDGT,IDGTN
      DATA IDGT /1H /
      DATA IDGTN/1H-/
*.___________________________________________
      ISIGNE=IDGT
      IEXP1=IDGT
      IEXP2=IDGT
      FACT=1.
      XMAX=VMAX(A,N)
      XMIN=VMIN(A,N)
      IF(ABS(XMAX).LT.ABS(XMIN))XMAX=XMIN
      XMAX=ABS(XMAX)
      IF(XMAX.EQ.0.)RETURN
*
  10  IF(XMAX.LT.10000.)GO TO 20
      ISIGNE=IDGTN
      FACT=FACT*10.
      XMAX=XMAX/10.
      GO TO 10
  20  IF(XMAX.GE.1.)GO TO 30
      FACT=FACT*10.
      XMAX=XMAX*10.
      GO TO 20
  30  IF(FACT.EQ.1.)RETURN
*
      I=LOG10(FACT)+0.0001
      CALL HBCDI(I,2,IB)
      IEXP1=IB(1)
      IEXP2=IB(2)
      IF(ISIGNE.EQ.IDGTN)FACT=1./FACT
      DO 40 I=1,N
  40  A(I)=A(I)*FACT
      END
