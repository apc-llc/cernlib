*
* $Id: igrap2.F,v 1.2 1997/11/24 15:29:51 couet Exp $
*
* $Log: igrap2.F,v $
* Revision 1.2  1997/11/24 15:29:51  couet
* - protection added against possible division by 0
*
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/03 30/09/91  14.46.39  by  O.Couet
*-- Author :
      SUBROUTINE IGRAP2(K,AZ,BZ,E2,X,Y,MAXIT)
*.===========>
*.
*.   Underlaying routine for IGHIST
*. This subroutine finds a real zero of the continuous real
*. function Y(X) in a given interval (A,B). See accompanying
*. notes for details of the argument list and calling sequence
*.
*.        Modified by - P. Ward     Date - 24. 8.1973
*.
*..==========>
*
      SAVE A,B,J1,IT,YA,YTEST,Y1,X1,J3,J2,H
*.______________________________________
*
      IF(K.GT.0) GOTO 30
*
*              Calculate Y(X) at X=AZ.
*
      A=AZ
      B=BZ
      X=A
      J1=1
      IT=1
   10 K=J1
   20 RETURN
*
*              Test whether Y(X) is sufficiently small.
*
   30 IF(ABS(Y).GT.E2)GOTO 50
   40 K=2
      GOTO 20
   50 GOTO(60,70,100,170),J1
*
*              Calculate Y(X) at X=BZ.
*
   60 YA=Y
      X=B
      J1=2
      GOTO 20
*
*              Test whether the signs of Y(AZ) and Y(BZ) are different.
*              if not, begin the binary subdivision.
*
   70 IF(YA*Y.LT.0.)GOTO 120
      X1=A
      Y1=YA
      J1=3
      H=B-A
      J2=1
   80 X2=A+0.5*H
      J3=1
*
*              Check whether (MAXIT) function values have been calculated.
*
   90 IT=IT+1
      IF(IT.GE.MAXIT)GOTO 10
      X=X2
      GOTO 20
*
*              Test whether a bracket has been found .
*              If not,continue the search
*
  100 IF(YA*Y.LT.0.)GOTO 120
      IF(J3.GE.J2)GOTO 110
      A=X
      YA=Y
      X2=X+H
      J3=J3+1
      GOTO 90
  110 A=X1
      YA=Y1
      H=0.5*H
      J2=2.*J2
      GOTO 80
*
*              The first bracket has been found.calculate the next X by the
*              secant method based on the bracket.
*
  120 B=X
      YB=Y
      J1=4
  130 IF(ABS(YA).LE.ABS(YB))GOTO 140
      X1=A
      Y1=YA
      X=B
      Y=YB
      GOTO 150
  140 X1=B
      Y1=YB
      X=A
      Y=YA
*
*              Use the secant method based on the function values Y1 and Y.
*              check that X2 is inside the interval (A,B).
*
  150 IF (Y.EQ.Y1) GOTO 160
      X2=X-Y*(X-X1)/(Y-Y1)
      X1=X
      Y1=Y
      YTEST=0.5*MIN(ABS(YA),ABS(YB))
      IF((X2-A)*(X2-B).LT.0.)GOTO 90
*
*              Calculate the next value of X by bisection . Check whether
*              the maximum accuracy has been achieved.
*
  160 X2=0.5*(A+B)
      YTEST=0.
      IF((X2-A)*(X2-B))90,40,40
*
*              Revise the bracket (A,B).
*
  170 IF(YA*Y.GE.0.)GOTO 180
      B=X
      YB=Y
      GOTO 190
  180 A=X
      YA=Y
*
*              Use YTEST to decide the method for the next value of X.
*
  190 IF(YTEST.LE.0.)GOTO 130
      IF(ABS(Y)-YTEST)150,150,160
*
      END
