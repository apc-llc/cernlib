*
* $Id: sigop1.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigop1.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGOP1(A,X)
C
#include "sigma/sigc.inc"
C
C    ....................................................
C         PURPOSE
C            PROCESS SYSFUN CODE 1 TO 12
C
C         USAGE
C            CALL OP1(A,X)
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: IZMODB
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            INTERNAL FUNCTIONS: SIN,COS,SQRT,EXP,LOG,
C                                ATAN,ABS,INT,LOG10,TANH
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................

C
C   RANGE OF KLASS CODES ALREADY CHECKED BY SYSFUN
      GO TO(1,2,3,4,5,6,7,10,11,12),KLASS
    1 CONTINUE
C
C******** 01     ************
C 001    SIN
C
C     CHECK SIZE OF SIN ARGUMENT
C
      X=0.0
      IF(ABS(A).LT.2**18)X=SIN(A)
      GOTO 11111
C
    2 CONTINUE
C
C******** 02     ************
C 002
C
C     CHECK SIZE OF COS ARGUMENT
C
      X=0.0
      IF(ABS(A).LT.2**18)X=COS(A)
      GOTO 11111
C
    3 CONTINUE
C
C******** 03     ************
C 003    SQRT
      X=0.
      IF(A.LT.0.)RETURN
      X=SQRT(A)
      GOTO 11111
C
    4 CONTINUE
C
C******** 04     ************
C 004    EXP
C
C     CHECK SIZE OF EXP ARG
C
      X=0.0
      IF((A.LE.75.).AND.(A.GE.-75.)) X=EXP(A)
      GOTO 11111
C
    5 CONTINUE
C
C******** 05     ************
C 005    LOG
      X=0.
      IF(A.LT.0.)RETURN
      IF(A.GT.0) X=ALOG(A)
CEV   IF(A.EQ.0) X=0.
      GOTO 11111
C
    6 CONTINUE
C
C******** 06     ************
C 006    ATAN
      X=ATAN(A)
      GOTO 11111
C
    7 CONTINUE
C******** 07     ************
C 007    ABS
C
      X=ABS(A)
      RETURN
C
   10 CONTINUE
C******** 10     ************
C 010    INT
C
      X=AINT(A)
      GOTO 11111
C
   11 CONTINUE
C******** 11     ************
C 011    LOG10
      X=0.
      IF(A.LT.0.)RETURN
C     SEE ALOG ABOVE
      IF(A.GT.0) X=ALOG10(A)
CEV   IF(A.EQ.0) X=0.
      GOTO 11111
C
   12 CONTINUE
C******** 12     ************
C 012    TANH
      X=TANH(A)
      GOTO 11111
C
C
C
C
11111 CONTINUE
C
 999  END
