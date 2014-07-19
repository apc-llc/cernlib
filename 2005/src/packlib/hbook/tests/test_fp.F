*
* $Id: test_fp.F,v 1.1.1.1 1996/01/16 17:08:12 mclareni Exp $
*
* $Log: test_fp.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:12  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
#if defined(CERNLIB_TESTS)
*CMZ :          21/06/95  14.24.56  by  Julian Bunn
*-- Author :
 
****************************************************************
*program test fortran for double precision and floating points *
****************************************************************
 
      PROGRAM FPMAIN
      INTEGER DENORM , SIGNAL_NAN
      DOUBLE PRECISION M , D
      REAL R , L , K
 
*********************************************
*test the accurate classification of doubles*
*********************************************
 
      PRINT '(A)' , 'THE TEST CONCERNING IFPDCL IS STARTING'
      IFLAG=1
*positive normalized 0
      M =10.
      I = IFPDCL(M)
      IF (I .EQ. 0) THEN
           PRINT '(A)', ' test on positive normalized succeeds'
      ELSE
           PRINT '(A)' ,' test on positive normalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative normalized 1
        D = -10.
        I = IFPDCL(D)
        IF (I .EQ. 1) THEN
           PRINT '(A)', ' test on negative normalized succeeds'
        ELSE
           PRINT '(A)' ,' test on negative normalized doesn t succeed'
           IFLAG=0
        ENDIF
 
*positive zero 2
        D = 0.
        I = IFPDCL(D)
        IF (I .EQ. 2) THEN
           PRINT '(A)', ' test on positive zero succeeds'
        ELSE
           PRINT '(A)' ,' test on positive zero doesn t succeed'
           IFLAG=0
        ENDIF
 
*negative zero 3
        M = -1.
        DO 1 I = 1,100
           J =  IFPDCL(M)
           M = M /1000000000000000.
 1      CONTINUE
        IF (J .EQ. 3) THEN
           PRINT '(A)', ' test on negative zero succeeds'
        ELSE
           PRINT '(A)' ,' test on negative zero doesn t succeed'
           IFLAG=0
        ENDIF
 
*positive infinity 4
        M = 1.
        DO 2 I = 1,100
           M = M * 1000000000000000.
           J =  IFPDCL(M)
 2      CONTINUE
        IF (J .EQ. 4) THEN
           PRINT '(A)', ' test on positive infinity succeeds'
        ELSE
           PRINT '(A)' ,' test on positive infinity doesn t succeed'
           IFLAG=0
        ENDIF
 
*negative infinity 5
        M = -1.
        DO 3 I = 1,100
           J =  IFPDCL(M)
           M = M *100000000000000000.
3      CONTINUE
       IF (J .EQ. 5) THEN
          PRINT '(A)', ' test on negative infinity succeeds'
       ELSE
          PRINT '(A)' ,' test on negative infinity doesn t succeed'
          IFLAG=0
       ENDIF
 
*positive denormalized 6
*approximation when you pass a parameter < 10**-45
*impossible to have a subnormal number
       J=0
       I = DENORM(J)
       IF (I .EQ. 6) THEN
          PRINT '(A)', ' test on positive denormalized succeeds'
       ELSE
          PRINT '(A)' ,' test on positive denormalized doesn t succeed'
          IFLAG=0
       ENDIF
 
*negative denormalized 7
       J = 1
       I = DENORM(J)
       IF (I .EQ. 7) THEN
          PRINT '(A)', ' test on negative denormalized succeeds'
       ELSE
          PRINT '(A)' ,' test on negative denormalized doesn t succeed'
          IFLAG=0
       ENDIF
 
*signaling nan 8
       I = SIGNAL_NAN()
       IF (I .EQ. 8) THEN
          PRINT '(A)', ' test on signaling nan succeeds'
       ELSE
          PRINT '(A)' ,' test on signaling nan doesn t succeed'
          IFLAG=0
       ENDIF
 
 
*quiet nan 9
       D = M * 0.
       I = IFPDCL(D)
       IF (I .EQ. 9) THEN
          PRINT '(A)', ' test on quiet nan succeeds'
       ELSE
          PRINT '(A)' ,' test on quiet nan doesn t succeed'
          IFLAG=0
       ENDIF
 
       IF(IFLAG .EQ. 1) THEN
          PRINT '(A)', 'TEST ON IFPDCL IS FINISHED AND SUCCEEDS'
       ELSE
          PRINT '(A)','TEST ON IFPDCL IS FINISHED AND DOESN T SUCCEED'
       ENDIF
       PRINT '(A)', ' '
 
********************************************
*test the accurate classification of floats*
********************************************
 
      PRINT '(A)' ,'THE TEST CONCERNING IFPSCL IS STARTING'
      IFLAG=1
*positive normalized 0
      K =10.
      J = IFPSCL(K)
      IF (J .EQ. 0) THEN
           PRINT '(A)', ' test on positive normalized succeeds'
      ELSE
           PRINT '(A)' ,' test on positive normalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative normalized 1
      K = -10.
      J = IFPSCL(K)
      IF (J .EQ. 1) THEN
           PRINT '(A)', ' test on negative normalized succeeds'
      ELSE
           PRINT '(A)' ,' test on negative normalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*positive zero 2
      K = 0.
      J = IFPSCL(K)
      IF (J .EQ. 2) THEN
           PRINT '(A)', ' test on positive zero succeeds'
      ELSE
           PRINT '(A)' ,' test on positive zero doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative zero 3
      R = -1.
      DO 10 I = 1,100
         J = IFPSCL(R)
         R = R /1000000000000000.
   10 CONTINUE
      IF (J .EQ. 3) THEN
           PRINT '(A)', ' test on negative zero succeeds'
      ELSE
           PRINT '(A)' ,' test on negative zero doesn t succeed'
           IFLAG=0
      ENDIF
 
*positive infinity 4
      R = 1.
      DO 20 I = 1,100
         R = R * 1000000000000000.
         J = IFPSCL(R)
   20 CONTINUE
      IF (J .EQ. 4) THEN
           PRINT '(A)', ' test on positive infinity succeeds'
      ELSE
           PRINT '(A)' ,' test on positive infinity doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative infinity 5
      L = -1.
      DO 30 I = 1,100
         J = IFPSCL(L)
         L = L *100000000000000000.
   30 CONTINUE
      IF (J .EQ. 5) THEN
           PRINT '(A)', ' test on negative infinity succeeds'
      ELSE
           PRINT '(A)' ,' test on negative infinity doesn t succeed'
           IFLAG=0
      ENDIF
#if defined(CERNLIB_HPUX)||defined(CERNLIB_SGI)
*positive denormalized 6
 
      K = 0.1*2.**(-126.)
      J = IFPSCL(K)
      IF (J .EQ. 6) THEN
           PRINT '(A)', ' test on positive denormalized succeeds'
      ELSE
           PRINT '(A)' ,' test on positive denormalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative denormalized 7
      K = -0.1*2.**(-126.)
      J = IFPSCL(K)
      IF (J .EQ. 7) THEN
           PRINT '(A)', ' test on negative denormalized succeeds'
      ELSE
           PRINT '(A)' ,' test on negative denormalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*signaling nan 8
*I pass a quiet nan in a double variable
      M = R * 0.
      J = IFPSCL(M)
      IF (J .EQ. 8) THEN
           PRINT '(A)', ' test on signaling nan succeeds'
      ELSE
           PRINT '(A)' ,' test on signaling nan doesn t succeed'
           IFLAG=0
      ENDIF
 
#endif
*quiet nan 9
      K = R * 0.
      J = IFPSCL(K)
      IF (J .EQ. 9) THEN
           PRINT '(A)', ' test on quiet nan succeeds'
      ELSE
           PRINT '(A)' ,' test on quiet nan doesn t succeed'
           IFLAG=0
      ENDIF
 
      IF(IFLAG .EQ. 1) THEN
          PRINT '(A)', 'TEST ON IFPSCL IS FINISHED AND SUCCEEDS'
      ELSE
          PRINT '(A)','TEST ON IFPSCL IS FINISHED AND DOESN T SUCCEED'
      ENDIF
      PRINT '(A)', ' '
 
*******************************************
*test the simple classification of doubles*
*******************************************
 
      PRINT '(A)' ,'THE TEST CONCERNING IFPD IS STARTING'
      IFLAG=1
*positive normalized 1
      M =10.
      I = IFPD(M)
      IF (I .EQ. 1) THEN
           PRINT '(A)', ' test on positive normalized succeeds'
      ELSE
           PRINT '(A)' ,' test on positive normalized doesn t succeed'
           IFLAG=0
      ENDIF
 
 
*negative normalized 1
        D = -10.
        I = IFPD(D)
        IF (I .EQ. 1) THEN
           PRINT '(A)', ' test on negative normalized succeeds'
        ELSE
           PRINT '(A)' ,' test on negative normalized doesn t succeed'
           IFLAG=0
        ENDIF
 
*positive zero 1
        D = 0.
        I = IFPD(D)
        IF (I .EQ. 1) THEN
           PRINT '(A)', ' test on positive zero succeeds'
        ELSE
           PRINT '(A)' ,' test on positive zero doesn t succeed'
           IFLAG=0
        ENDIF
 
*negative zero 1
        M = -1.
        DO 100 I = 1,100
           J =  IFPD(M)
           M = M /1000000000000000.
 100    CONTINUE
        IF (J .EQ. 1) THEN
           PRINT '(A)', ' test on negative zero succeeds'
        ELSE
           PRINT '(A)' ,' test on negative zero doesn t succeed'
           IFLAG=0
        ENDIF
 
*positive infinity 0
        M = 1.
        DO 200 I = 1,100
           M = M * 1000000000000000.
           J =  IFPD(M)
 200    CONTINUE
        IF (J .EQ. 0) THEN
           PRINT '(A)', ' test on positive infinity succeeds'
        ELSE
           PRINT '(A)' ,' test on positive infinity doesn t succeed'
           IFLAG=0
        ENDIF
 
*negative infinity 0
        M = -1.
        DO 300 I = 1,100
           J =  IFPD(M)
           M = M *100000000000000000.
 300   CONTINUE
       IF (J .EQ. 0) THEN
          PRINT '(A)', ' test on negative infinity succeeds'
       ELSE
          PRINT '(A)' ,' test on negative infinity doesn t succeed'
          IFLAG=0
       ENDIF
 
 
*quiet nan 0
       D = M * 0.
       I = IFPD(D)
       IF (I .EQ. 0) THEN
          PRINT '(A)', ' test on quiet nan succeeds'
       ELSE
          PRINT '(A)' ,' test on quiet nan doesn t succeed'
          IFLAG=0
       ENDIF
 
       IF(IFLAG .EQ. 1) THEN
          PRINT '(A)', 'TEST ON IFPD IS FINISHED AND SUCCEEDS'
       ELSE
          PRINT '(A)','TEST IFPD IS FINISHED AND DOESN T SUCCEED'
       ENDIF
       PRINT '(A)', ' '
 
*******************************************
*test the simple classification of floats*
*******************************************
 
      PRINT '(A)' ,'THE TEST CONCERNING IFPS IS STARTING'
      IFLAG=1
*positive normalized 1
      K =10.
      J = IFPS(K)
      IF (J .EQ. 1) THEN
           PRINT '(A)', ' test on positive normalized succeeds'
      ELSE
           PRINT '(A)' ,' test on positive normalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative normalized 1
      K = -10.
      J = IFPS(K)
      IF (J .EQ. 1) THEN
           PRINT '(A)', ' test on negative normalized succeeds'
      ELSE
           PRINT '(A)' ,' test on negative normalized doesn t succeed'
           IFLAG=0
      ENDIF
 
*positive zero 1
      K = 0.
      J = IFPS(K)
      IF (J .EQ. 1) THEN
           PRINT '(A)', ' test on positive zero succeeds'
      ELSE
           PRINT '(A)' ,' test on positive zero doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative zero 1
      R = -1.
      DO 1000 I = 1,100
         J = IFPS(R)
         R = R /1000000000000000.
 1000 CONTINUE
      IF (J .EQ. 1) THEN
           PRINT '(A)', ' test on negative zero succeeds'
      ELSE
           PRINT '(A)' ,' test on negative zero doesn t succeed'
           IFLAG=0
      ENDIF
 
*positive infinity 0
      R = 1.
      DO 2000 I = 1,100
         R = R * 1000000000000000.
         J = IFPS(R)
 2000 CONTINUE
      IF (J .EQ. 0) THEN
           PRINT '(A)', ' test on positive infinity succeeds'
      ELSE
           PRINT '(A)' ,' test on positive infinity doesn t succeed'
           IFLAG=0
      ENDIF
 
*negative infinity 0
      L = -1.
      DO 3000 I = 1,100
         J = IFPS(L)
         L = L *100000000000000000.
 3000 CONTINUE
      IF (J .EQ. 0) THEN
           PRINT '(A)', ' test on negative infinity succeeds'
      ELSE
           PRINT '(A)' ,' test on negative infinity doesn t succeed'
           IFLAG=0
      ENDIF
 
 
*signaling nan 0
*I pass a quiet nan in a double variable
      M = R * 0.
      J = IFPS(M)
      IF (J .EQ. 0) THEN
           PRINT '(A)', ' test on signaling nan succeeds'
      ELSE
           PRINT '(A)' ,' test on signaling nan doesn t succeed'
           IFLAG=0
      ENDIF
 
 
*quiet nan 0
      K = R * 0.
      J = IFPS(K)
      IF (J .EQ. 0) THEN
           PRINT '(A)', ' test on quiet nan succeeds'
      ELSE
           PRINT '(A)' ,' test on quiet nan doesn t succeed'
           IFLAG=0
      ENDIF
 
      IF(IFLAG .EQ. 1) THEN
          PRINT '(A)', 'TEST ON IFPS IS FINISHED AND SUCCEEDS'
      ELSE
          PRINT '(A)','TEST ON IFPS IS FINISHED AND DOESN T SUCCEED'
      ENDIF
 
      END
 
 
 
 
 
 
#endif
