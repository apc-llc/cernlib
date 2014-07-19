*
* $Id: d503m.F,v 1.1.1.1 1996/04/01 15:01:24 mclareni Exp $
*
* $Log: d503m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:24  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D503M
C     This program tests the operation of MATHLIB subroutine subprograms
C     RMINFC and DMINFC (D503).

#include "gen/imp64.inc"
      LOGICAL LLM
      EXTERNAL D503F1,D503F2,D503F3,D503F4,D503F5,D503F6
      EXTERNAL D503F7,D503F8,D503F9
      DIMENSION Z(2)

      PARAMETER (Z0 = 0, Z1 = 1)
C     Set maximum error allowed for test to be considered successful
      PARAMETER ( TSTERR=5D-8 )

#include "iorc.inc"

      CALL HEADER('D503',0)
      ERRMAX=0D0

C     CASE 1

      IC=1
      EPS=1D-8
      DELTA=10*EPS

       CALL DBZEJY(Z1,1,3,1D-14,Z)
       T=Z(1)
       A=5
       B=1
      FA=D503F1(A)
      FB=D503F1(B)
      CALL DMINFC(D503F1,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(/6X,''A'',8X,''B'',9X,''X'',15X,''T'',15X,
     +             ''FA'',10X,''Y'',10X,''FB'',6X,''Error'',
     +             6X,''EPS'',4X,''LLM'',2X,''IC''//)')
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 2

      IC=2
       T=0.5D0
       A=0.25D0
       B=0.75D0
      FA=D503F2(A)
      FB=D503F2(B)
      CALL DMINFC(D503F2,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 3

      IC=3
       T=-1
       A=-2
       B=-0.5D0
      FA=D503F3(A)
      FB=D503F3(B)
      CALL DMINFC(D503F3,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 4

      IC=4
       T=1+SQRT(2D0)
       A=1
       B=4
      FA=D503F4(A)
      FB=D503F4(B)
      CALL DMINFC(D503F4,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 5

      IC=5
       T=0
       A=-1
       B=1.5D0
      FA=D503F5(A)
      FB=D503F5(B)
      CALL DMINFC(D503F5,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 6

      IC=6
       CALL DBZEJY(Z0,2,3,1D-14,Z)
       T=Z(2)
       A=2
       B=12
      FA=D503F6(A)
      FB=D503F6(B)
      CALL DMINFC(D503F6,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 7

      IC=7
       T=1/SQRT(3D0)
       A=0
       B=2
      FA=D503F7(A)
      FB=D503F7(B)
      CALL DMINFC(D503F7,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 8

      IC=8
       CALL DBZEJY(Z1,1,3,1D-14,Z)
       T=Z(1)/10
       A=0
       B=0.20D0
      FA=D503F8(A)
      FB=D503F8(B)
      CALL DMINFC(D503F8,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC

C     CASE 9

      IC=9
       T=1
       A=0
       B=3
      FA=D503F9(A)
      FB=D503F9(B)
      CALL DMINFC(D503F9,A,B,EPS,DELTA,X,Y,LLM)
      ERRMAX = MAX (ERRMAX,ABS((T-X)/(1+ABS(T))))
      WRITE(LOUT,'(1X,2F8.2,2F16.12,3F12.6,1P,2D10.1,L3,I3//)')
     1          A,B,X,T,FA,Y,FB,ABS((T-X)/(1+ABS(T))),EPS,LLM,IC
      WRITE(LOUT,'(/'' Largest Relative Error was'',1P,D10.1)') ERRMAX
C     Check if the test was successful
      IRC=ITEST('D503',ERRMAX .LE. TSTERR)
      CALL PAGEND('D503')
      RETURN
      END
      FUNCTION D503F(X)
#include "gen/imp64.inc"

      ENTRY D503F1(X)

      D503F1=-DBESJ1(X)
      RETURN

      ENTRY D503F2(X)

      D503F2=4*X**2+1/X
      RETURN

      ENTRY D503F3(X)

      D503F3=-EXP(X+1/X)
      RETURN

      ENTRY D503F4(X)

      D503F4=(X**2-5*X+6)/(X**2+1)
      RETURN

      ENTRY D503F5(X)

      D503F5=-COS(X)+LOG(COSH(X))
      RETURN

      ENTRY D503F6(X)

      D503F6=DBESJ0(X)
      RETURN

      ENTRY D503F7(X)

      D503F7=X*(X**2-1)
      RETURN

      ENTRY D503F8(X)

      D503F8=-DBESJ1(10*X)
      RETURN

      ENTRY D503F9(X)

      D503F9=ABS(X-1)
      RETURN
      END
