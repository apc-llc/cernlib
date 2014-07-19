*
* $Id: hexam5.F,v 1.2 1996/06/06 09:52:43 cernlib Exp $
*
* $Log: hexam5.F,v $
* Revision 1.2  1996/06/06 09:52:43  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
#include "pilot.h"
*CMZ :  4.10/05 22/11/89  18.48.27  by  Rene Brun
*-- Author :
      SUBROUTINE HEXAM5
*.==========>
*.           OPERATIONS ON HISTOGRAMS AND FITTING
*..=========> ( R.Brun )
      COMMON/HDEXF/C1,C2,XM1,XM2,XS1,XS2
      COMMON /HEXF/A,B,C,D
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION C1,C2,XM1,XM2,XS1,XS2,HDFUN,HDFUN1
#endif
      DIMENSION X(100),Y(100)
      DIMENSION XF(4000,2),YF(4000),EY(4000),SIGPAR(6),COV(21),ST(6),
     +PMI(6),PMA(6)
      EXTERNAL HDFUN,HDFUN1,HFUNGA
      CHARACTER*12 TITL1
      DATA TITL1/'TITLE OF ID1'/
*.___________________________________________
*.
      CALL HTITLE('EXAMPLE NO = 5')
*
*             GET hist 110 from data base
*
      CALL HRGET(110,'hexam.dat',' ')
      CALL HRGET(210,'hexam.dat',' ')
*
*
      CALL HBOOK1(1,TITL1,100,0.,1.,0.)
      CALL HCOPY(1,2,'TITLE OF ID = 2')
*
*             Gets information from ID=110 and fills new IDs 1,2
*
      CALL HUNPAK(110,X,'HIST',1)
      CALL UCOPY(X,Y,100)
      CALL VZERO(X(51),50)
      CALL HPAK(1,X)
      CALL HPHIST(1,'HIST',1)
      CALL VZERO(Y,50)
      CALL HPAK(2,Y)
      CALL HPHIST(2,'HIST',1)
*
*             adds 1 and 2. Identifier 3 is created and will contain
*             result of addition
*
      CALL HOPERA(1,'+',2,3,1.,1.)
      CALL HCOPY(3,4,' ')
*
*             Fits 3 with the function HTFUN1 defined in example 2 .
*             Initializes parameters. Prints results of the last
*             iteration.
*             Superimpose result of fit to the histogram
*             The result of this fit can be compared with the initial
*             parameters of example 2
*
      C1=40.
      C2=20.
      XM1=0.4
      XM2=0.6
      XS1=0.1
      XS2=0.1
*
      CALL HFITS(3,HDFUN1,6,C1,CHI2,12,SIGPAR)
*
      CALL HPHIST(3,'HIST',1)
*
*
*            Fits a two-dimensional distribution (xf,yf) with HFITN
*            initialize parameters. Prints results of the last
*            iteration.
*            Errors EY automatically computed as SQRT(yf)
*
      NY=0
      DO 10 J=1,40
         DO 5 I=1,100
            CONT=HIJ (210,I,J)
            IF (CONT.EQ.0.) GOTO 5
            NY=NY+1
            YF(NY)=CONT
            EY(NY)=SQRT(CONT)
            CALL HIJXY (210,I,J,X1,X2)
            XF(NY,1)=X1+0.005
            XF(NY,2)=X2+0.0125
    5    CONTINUE
   10 CONTINUE
      C1=3.
      C2=1.
      XM1=0.3
      XM2=0.7
      XS1=0.07
      XS2=0.12
      DO 15 I=1,6
         ST(I)=-1.
   15 CONTINUE
*
      CALL HFITN (XF,YF,EY,NY,4000,2,HDFUN,6,C1,CHI2,11,SIGPAR,COV,
     +ST,PMI,PMA)
      WRITE(31,*) ' COVARIANCE MATRIX'
      WRITE(31,*) ' *****************'
      I2=0
      DO 20 K=1,6
         I1=I2+1
         I2=I1+K-1
         WRITE(31,*) (COV(I),I=I1,I2)
   20 CONTINUE
*
*
*       Gaussian fitting. Prints first and last iterations.
*
      CALL HDELET (0)
      A=2.
      B=0.4
      C=0.1
      CALL HBFUN1 (1,' ',100,0.,1.,HFUNGA)
      CALL HBOOK1 (5,' ',100,0.,1.,1000.)
      DO 30 I=1,5000
         XR=HRNDM1 (1)
         CALL HFILL (5,XR,0.,1.)
   30 CONTINUE
*
      CALL HFITGA (5,A,B,C,CHI2,12,SIGPAR)
      CALL HPRINT (5)
      CALL HDELET (0)
*
      END
*
*
#if !defined(CERNLIB_DOUBLE)
      FUNCTION HDFUN (X)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNCTION HDFUN (X)
      DOUBLE PRECISION PAR,DER,DER1,HDFUN1,F1,F2
#endif
      DIMENSION DER(6),DER1(6),X(2)
      COMMON/HDEXF/PAR(6)
*
*         Compute value of the function at point X
*
      F1=HDFUN1(X(1))
      F2=HDFUN1(X(2))
      HDFUN=F1+F2
*
*         Compute derivatives
*
*      CALL HDERU1 (X(1),PAR,DER)
*      CALL HDERU1 (X(2),PAR,DER1)
*      DO 10 K=1,6
*         DER(K)=DER(K)+DER1(K)
*   10 CONTINUE
*
*      CALL HDERIV(DER)
*
      END
*
*
*
      SUBROUTINE HDERU1 (X,PAR,DER)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION PAR,DER
#endif
      DIMENSION PAR(6),DER(6)
      DER(1)=EXP(-0.5*((X-PAR(3))/PAR(5))**2)
      DER(2)=EXP(-0.5*((X-PAR(4))/PAR(6))**2)
      DER(3)=PAR(1)*(X-PAR(3))/PAR(5)**2*DER(1)
      DER(4)=PAR(2)*(X-PAR(4))/PAR(6)**2*DER(2)
      DER(5)=DER(3)*(X-PAR(3))/PAR(5)
      DER(6)=DER(4)*(X-PAR(4))/PAR(6)
      END
*
*
      FUNCTION HFUNGA (X)
      COMMON /HEXF/ A,B,C,D
      HFUNGA=A*EXP(-0.5*((X-B)/C)**2)
      END
