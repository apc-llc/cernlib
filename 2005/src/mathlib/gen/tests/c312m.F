*
* $Id: c312m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c312m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C312M
C     This program tests the operation of KERNLIB subprograms
C     BESJ0, BESJ1, BESY0, BESY1 and
C     DBESJ0, DBESJ1, DBESY0, DBESY1  (C312)

#include "imp64r.inc"
      REAL BESJ0, BESJ1, BESY0, BESY1
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)


      LOGICAL LTEST
C     Set the total number of tests for BEJ0 and BESJ1
      PARAMETER ( NT=5)
#if defined(CERNLIB_DOUBLE)
      DIMENSION X(NT),EX(NT,4),ER(NT,4)
      REAL     RX(NT),REX(NT,4),RER(NT,4)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL X(NT),EX(NT,4),ER(NT,4)
#endif
#include "iorc.inc"
      DATA TOL/1D-6, 1D-13/
C     Input parameters for individual tests
       DATA X( 1 )/ -10.0D0/
       DATA X( 2 )/  -2.0D0/
       DATA X( 3 )/  0.0D0/
       DATA X( 4 )/  1.0D0/
       DATA X( 5 )/  9.0D0/
C     Analytical values expected to be obtained
        DATA (EX( 1,J ),J=1,4)
     +               /-0.245935764451348D0,-0.043472746168861D0,
     +                 0.000000000000000D0, 0.000000000000000D0 /
        DATA (EX( 2,J ),J=1,4)
     +               / 0.223890779141235D0,-0.576724807756873D0,
     +                 0.000000000000000D0, 0.000000000000000D0 /
        DATA (EX( 3,J ),J=1,4)
     +               /0.999999999999999D0, 0.000000000000000D0,
     +                0.000000000000000D0, 0.000000000000000D0 /
        DATA (EX( 4,J ),J=1,4)
     +               / 0.765197686557966D0, 0.440050585744933D0,
     +                 0.088256964215678D0,-0.781212821300289D0/
        DATA (EX( 5,J ),J=1,4)
     +               /-0.090333611182876D0, 0.245311786573325D0,
     +                 0.249936698285025D0, 0.104314575196716D0/

      DATA LTEST/.TRUE./
      CALL HEADER('C312',0)
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR BESJ0, BESJ1,'',
     +'' BESY0, BESY1'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR BESJ0, BESJ1,'',
     +'' BESY0, BESY1'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DBESJ0,'',
     +'' DBESJ1, DBESY0, DBESY1'')')
#endif
      WRITE(LOUT,'(/9X,''X'',15X,''BJ0'',21X,''BJ1'')')
      DO 1 I = 1,5
#if !defined(CERNLIB_DOUBLE)
      BJ0=BESJ0(X(I))
      BJ1=BESJ1(X(I))
      ER(I,1)= ABS(EX(I,1)-BJ0 )
      ER(I,2)= ABS(EX(I,2)-BJ1 )
      DO 7 J = 1,2
         ERRMAX = MAX( ERRMAX ,ER(I,J) )
    7 CONTINUE
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                RX(I)=X(I)
      BJ0= BESJ0(RX(I))
      BJ1= BESJ1(RX(I))
      DO 222 J = 1,4
                REX(I,J)=EX(I,J)
  222 CONTINUE
      RER(I,1)= ABS(REX(I,1)-BJ0 )
      RER(I,2)= ABS(REX(I,2)-BJ1 )
      DO 666 J = 1,2
                 ER(I,J)=RER(I,J)
  666 CONTINUE
      ENDIF
      IF(JF.EQ.2)THEN
      BJ0=DBESJ0(X(I))
      BJ1=DBESJ1(X(I))
      ER(I,1)= ABS(EX(I,1)-BJ0 )
      ER(I,2)= ABS(EX(I,2)-BJ1 )
      ENDIF
      DO 8 J = 1,2
         ERRMAX = MAX( ERRMAX ,ER(I,J) )
    8 CONTINUE
#endif
      WRITE(LOUT,'(1X,F10.1,2F25.15)') X(I),BJ0,BJ1
   1  CONTINUE
      WRITE(LOUT,'(/9X,''X'',15X,''BY0'',21X,''BY1'')')
      DO 11 I =4,5
      BY0=0
      BY1=0
#if !defined(CERNLIB_DOUBLE)
       BY0= BESY0(X(I))
       BY1= BESY1(X(I))
      ER(I,3)= ABS(EX(I,3)-BY0 )
      ER(I,4)= ABS(EX(I,4)-BY1 )
      DO 9 J = 3,4
         ERRMAX = MAX( ERRMAX ,ER(I,J) )
    9 CONTINUE
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                RX(I)=X(I)
       BY0= BESY0(RX(I))
       BY1= BESY1(RX(I))
      RER(I,3)= ABS(REX(I,3)-BY0 )
      RER(I,4)= ABS(REX(I,4)-BY1 )
      DO 333 J = 3,4
                 ER(I,J)=RER(I,J)
  333 CONTINUE
      ENDIF
      IF(JF.EQ.2)THEN
       BY0=DBESY0(X(I))
       BY1=DBESY1(X(I))
      ER(I,3)= ABS(EX(I,3)-BY0 )
      ER(I,4)= ABS(EX(I,4)-BY1 )
      ENDIF
      DO 2 J = 3,4
         ERRMAX = MAX( ERRMAX ,ER(I,J) )
    2 CONTINUE
      WRITE(LOUT,'(1X,F10.1,2F25.15)') X(I),BY0,BY1
#endif
   11 CONTINUE
      WRITE(LOUT,'(/''LARGEST RELATIVE ERROR WAS'',1P,D10.1)')
     +ERRMAX
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')
       DO 444 I=2,3
#if !defined(CERNLIB_DOUBLE)
       BY0= BESY0(X(I))
       BY1= BESY1(X(I))
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX(I)=X(I)
       BY0= BESY0(RX(I))
       BY1= BESY1(RX(I))
      ENDIF
      IF(JF.EQ.2)THEN
       BY0=DBESY0(X(I))
       BY1=DBESY1(X(I))
      ENDIF
#endif
  444 CONTINUE
1000  CONTINUE
C     Check if the test was successful
      IRC=ITEST('C312',LTEST)

      CALL PAGEND('C312')
      END
