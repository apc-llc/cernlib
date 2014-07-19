*
* $Id: hplmak.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplmak.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.14/07 22/09/92  17.33.01  by  O.Couet
*-- Author :
      SUBROUTINE HPLMAK
*.==========>
*.           Creation of some histograms (based on HBOOK examples)
*..=========>
      COMMON/HEX2/C1,C2,XM1,XM2,XS1,XS2
      EXTERNAL HTFUN1,HTFUN2
*.___________________________________________
*
*             BOOKING
*
      C1=1.
      C2=0.5
      XM1=0.3
      XM2=0.7
      XS1=0.07
      XS2=0.12
*
      CALL HBFUN1(100,'TEST OF HRNDM1',100,0.,1.,HTFUN1)
*
      CALL HBOOK1(110,'Test of 1-DIM plots',100,0.,1.,1000.)
*
      CALL HBFUN2(200,'Test of 2-DIM plots',40,0.,1.,40,0.,1.,HTFUN2)
      CALL HSCALE(200,0.)
*
*             FILLING
*
      DO 10 I=1,5000
         X=HRNDM1(100)
         CALL HFILL(110,X,0.,1.)
  10  CONTINUE
*
      END
      FUNCTION HTFUN1(X)
      COMMON/HEX2/C1,C2,XM1,XM2,XS1,XS2
*
      A1=-0.5*((X-XM1)/XS1)**2
      A2=-0.5*((X-XM2)/XS2)**2
      X1=C1
      X2=C2
      IF(ABS(A1).GT.1.E-4)X1=C1*EXP(A1)
      IF(ABS(A2).GT.1.E-4)X2=C2*EXP(A2)
      HTFUN1=X1+X2
      END
      FUNCTION HTFUN2(X,Y)
      HTFUN2=HTFUN1(X)*HTFUN1(Y)
      END
