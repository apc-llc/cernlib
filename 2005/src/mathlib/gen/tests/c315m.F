*
* $Id: c315m.F,v 1.1.1.1 1996/04/01 15:01:15 mclareni Exp $
*
* $Log: c315m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:15  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C315M
C     This program tests the operation of MATHLIB subprograms
C     RRIZET and DRIZET (C315)

#include "imp64r.inc"
      REAL RRIZET
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)

      LOGICAL LTEST
C     Set the total number of tests
      PARAMETER ( NT=11)
#if defined(CERNLIB_DOUBLE)
      DIMENSION X(NT),EX(NT,2),ER(NT,2)
      REAL RX(NT),REX(NT,2)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL X(NT),EX(NT,2),ER(NT,2)
#endif
#include "iorc.inc"
      DATA TOL/1D-6, 1D-12/
      DATA LTEST/.TRUE./
C     Input parameters for individual tests
       DATA X( 1 )/   0.5D0/
c      DATA X( 2 )/   1.0D0/
       DATA X( 2 )/   3.0D0/
       DATA X( 3 )/  10.0D0/
       DATA X( 4 )/  20.0D0/
       DATA X( 5 )/  50.0D0/
       DATA X( 6 )/  60.0D0/
       DATA X( 7 )/  92.0D0/
       DATA X( 8 )/  -10.0D0/
       DATA X( 9 )/  -3.0D0/
       DATA X(10 )/  -0.5D0/
       DATA X(11 )/   1.0D0/

C     Analytical values expected to be obtained

       DATA (EX(1,J),J=1,2)
     + / -0.146035450880959D+01 , -0.146035450880959D+01/
       DATA (EX(11,J),J=1,2)
     + / 0.000000000000000D+00 ,  0.000000000000000D+00/
       DATA (EX(2,J),J=1,2)
     + / 0.120205690315959D+01 ,  0.202056903159594D+00/
       DATA (EX(3,J),J=1,2)
     + / 0.100099457512782D+01 ,  0.994575127818086D-03/
       DATA (EX(4,J),J=1,2)
     + / 0.100000095396203D+01 ,  0.953962033872798D-06/
       DATA (EX(5,J),J=1,2)
     + / 0.100000000000000D+01 ,  0.888178421093083D-15/
       DATA (EX(6,J),J=1,2)
     + / 0.100000000000000D+01 ,  0.867361738011993D-18/
       DATA (EX(7,J),J=1,2)
     + / 0.100000000000000D+01 ,  0.201948391736579D-27/
       DATA EX(8,2)
     + / 0.000000000000000D+00/
       DATA EX( 9,2)
     + / 0.833333333333333D-02/
       DATA EX(10,2)
     + /-0.207886224977354D+00/
       DATA (ER(I,1),I=1,10)/0,0,0,0,0,0,0,0,0,0/
       DATA (ER(I,2),I=1,10)/0,0,0,0,0,0,0,0,0,0/
      CALL HEADER('C315',0)
         ERRMAX= 0.0D0



      WRITE(LOUT,'(/9X,''RRIZET/DRIZET has the value of the Riemann Zeta
     + Function of X if X<1'')')
      WRITE(LOUT,'(9X,''and of the Zeta Function minus 1 if X>1'')')
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR RRIZET'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR RRIZET'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DRIZET'')')
#endif
      WRITE(LOUT,'(/9X,''X'',9X,
     +''FUNCTION'',15X,
     +''FUNCTION+1 if X>1'')')
      DO 1 I = 1,11
      IF(X(I) .EQ. 1)
     +WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')

#if !defined(CERNLIB_DOUBLE)
      DR=RRIZET(X(I))
      DR1=DR
      IF(X(I) .GT. 1) DR1=1+DR
      IF(DR .NE. 0) ER(I,2)=ABS(( EX(I,2)-DR )/DR)
      IF(DR1.NE. 0 .AND. X(I) .GT. 1) ER(I,1)=ABS((EX(I,1)-DR1)/DR1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX(I)=X(I)
                 REX(I,1)=EX(I,1)
                 REX(I,2)=EX(I,2)
                 DR=RRIZET(RX(I))
      DR1=DR
      IF(RX(I) .GT. 1) DR1=1+DR
      IF(DR .NE. 0) ER(I,2)=ABS((REX(I,2)-DR )/DR)
      IF(DR1.NE. 0 .AND. X(I) .GT. 1) ER(I,1)=ABS((REX(I,1)-DR1)/DR1)
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DRIZET(X(I))
      DR1=DR
      IF(X(I) .GT. 1) DR1=1+DR
      IF(DR .NE. 0) ER(I,2)=ABS(( EX(I,2)-DR )/DR)
      IF(DR1.NE. 0 .AND. X(I) .GT. 1) ER(I,1)=ABS((EX(I,1)-DR1)/DR1)
      ENDIF
#endif
      DO 2 J = 1,2
         ERRMAX= MAX( ERRMAX,ER(I,J) )
    2 CONTINUE

      IF(X(I) .GE. 0)WRITE(LOUT,'(1X,F10.1,1P,2D25.15)')
     +  X(I),DR,DR1
      IF(X(I) .LT. 0) WRITE(LOUT,'(1X,F10.1,1P,D25.15)')
     +X(I),DR
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''Largest Relative Error was'',
     +D10.1)') ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
1000  CONTINUE
C     Check if the test was successful
      IRC=ITEST('C315',LTEST)
      CALL PAGEND('C315')
      END
