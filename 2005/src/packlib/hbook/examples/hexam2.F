*
* $Id: hexam2.F,v 1.2 1996/06/06 09:52:41 cernlib Exp $
*
* $Log: hexam2.F,v $
* Revision 1.2  1996/06/06 09:52:41  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "pilot.h"
*CMZ :  4.10/05 22/11/89  18.48.26  by  Rene Brun
*-- Author :
      SUBROUTINE HEXAM2
*.==========>
*.           TEST OF SOME BOOKING OPTIONS USING HBOOK RANDOM
*.           NUMBER GENERATORS.
*..=========> ( R.Brun )
      COMMON/HDEXF/C1,C2,XM1,XM2,XS1,XS2
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION C1,C2,XM1,XM2,XS1,XS2
#endif
      EXTERNAL HTFUN1,HTFUN2
*.___________________________________________
*.
      CALL HTITLE('EXAMPLE NO = 2')
*
*             Booking
*
      C1=1.
      C2=0.5
      XM1=0.3
      XM2=0.7
      XS1=0.07
      XS2=0.12
*
      CALL HBFUN1(100,'TEST OF HRNDM1',100,0.,1.,HTFUN1)
      CALL HIDOPT(100,'STAR')
      CALL HCOPY(100,10,' ')
*
      CALL HBOOK1(110,  'THIS HISTOGRAM IS FILLED ACCORDING TO THE FUNCT
     +ION HTFUN1'
     +  ,100,0.,1.,1000.)
*
      CALL HBFUN2(200,'TEST OF HRNDM2',100,0.,1.,40,0.,1.,HTFUN2)
      CALL HSCALE(200,0.)
      CALL HCOPY(200,20,' ')
*
      CALL HBOOK2(210,'HIST FILLED WITH HFILL AND HRNDM2' ,100,0.,1.,
     +  40,0.,1.,30.)
*
*             Filling
*
      DO 10 I=1,5000
         X=HRNDM1(100)
         CALL HFILL(110,X,0.,1.)
         CALL HRNDM2(200,X,Y)
         CALL HFILL(210,X,Y,1.)
  10  CONTINUE
*
*             Save all histograms on file 'hexam.dat'
*
      CALL HRPUT(0,'hexam.dat','N')
*
      CALL HDELET(100)
      CALL HDELET(200)
*
*             Printing
*
      CALL HPRINT(0)
      END
 
 
      FUNCTION HTFUN1(X)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION HDFUN1
#endif
      HTFUN1=HDFUN1(X)
      END
 
 
      FUNCTION HTFUN2(X,Y)
      HTFUN2=HTFUN1(X)*HTFUN1(Y)
      END
 
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HDFUN1(X)
#endif
#if !defined(CERNLIB_DOUBLE)
      FUNCTION HDFUN1(X)
#endif
      COMMON/HDEXF/C1,C2,XM1,XM2,XS1,XS2
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION C1,C2,XM1,XM2,XS1,XS2,A1,A2,X1,X2
#endif
*
      A1=-0.5*((X-XM1)/XS1)**2
      A2=-0.5*((X-XM2)/XS2)**2
      IF(A1.LT.-20.)THEN
         X1=0.
      ELSEIF(A1.GT.20.)THEN
         X1=1.E5
      ELSE
         X1=C1*EXP(A1)
      ENDIF
      IF(A2.LT.-20.)THEN
         X2=0.
      ELSEIF(A2.GT.20.)THEN
         X2=1.E5
      ELSE
         X2=C2*EXP(A2)
      ENDIF
      HDFUN1=X1+X2
      END
