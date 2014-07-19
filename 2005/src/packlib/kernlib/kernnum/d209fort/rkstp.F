*
* $Id: rkstp.F,v 1.1.1.1 1996/02/15 17:48:47 mclareni Exp $
*
* $Log: rkstp.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:47  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE RKSTP(N,H,X,Y,SUB,W)
      DIMENSION Y(N),W(N,3)
      LOGICAL MFLAG,RFLAG
      EXTERNAL SUB
C
C     ******************************************************************
C
C     THIS SUBROUTINE REPLACES X BY X+H AND ADVANCES THE SOLUTION OF THE
C     SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM Y(X) TO Y(X+H)
C     USING A FIFTH-ORDER RUNGE-KUTTA METHOD.
C
C     SUB IS THE NAME OF A SUBROUTINE SUB(X,Y,F) WHICH SETS THE VECTOR F
C     TO THE DERIVATIVE AT X OF THE VECTOR Y.
C
C     W IS A WORKING-SPACE ARRAY, TREATED AS CONSISTING OF THREE CONSEC-
C     UTIVE WORKING VECTORS OF LENGTH N.
C
C     ******************************************************************
C
C  START.
      IF (N.LT.1) GO TO 5
      NLOCAL=N
      HLOCAL=H
      H2=0.5*HLOCAL
      H6=HLOCAL/6.
      XH=X+HLOCAL
      XH2=X+H2
      CALL SUB(X,Y,W(1,1))
      DO 1 J=1,NLOCAL
         W(J,2)=Y(J)+H2*W(J,1)
    1 CONTINUE
      CALL SUB(XH2,W(1,2),W(1,3))
      DO 2 J=1,NLOCAL
         W(J,1)=W(J,1)+2.*W(J,3)
         W(J,2)=Y(J)+H2*W(J,3)
    2 CONTINUE
      CALL SUB(XH2,W(1,2),W(1,3))
      DO 3 J=1,NLOCAL
         W(J,1)=W(J,1)+2.*W(J,3)
         W(J,2)=Y(J)+HLOCAL*W(J,3)
    3 CONTINUE
      CALL SUB(XH,W(1,2),W(1,3))
      DO 4 J=1,NLOCAL
         Y(J)=Y(J)+H6*(W(J,1)+W(J,3))
    4 CONTINUE
      X=XH
      RETURN
C
    5 CALL KERMTR('D209.1',LGFILE,MFLAG,RFLAG)
      IF(MFLAG) THEN
         IF( LGFILE.EQ.0 ) THEN
            WRITE(*,6) N
         ELSE
            WRITE(LGFILE,6) N
         ENDIF
      ENDIF
      IF(.NOT. RFLAG) CALL ABEND
      RETURN
    6 FORMAT( 7X, 24HSUBROUTINE RKSTP ... N =, I6,16H IS LESS THAN 1. )
      END
