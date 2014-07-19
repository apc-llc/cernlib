*
* $Id: sifam0.F,v 1.1.1.1 1995/12/12 14:36:16 mclareni Exp $
*
* $Log: sifam0.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:16  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  12.17.04  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIFAM0
C
C    ..................................................
C         PURPOSE
C            INTERPRETATION OF ALL FAMILY ZERO INTERNAL OPERATORS
C     FAMILY OF 000   CALCULATIONS - ARITHMETICAL AND LOGICAL
C
C         USAGE
C            CALL SIFAM0
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: CNAME,INAME(),IPOINT,ITYPE,KLASS
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SIASSI
C            SINDLF
C            KONST
C            SIVARI
C            SYSFUN
C            SYSOP
C            SILSKK
C            SINDXI
C            SINERR
C            NEXT
C            ACTUAL
C            SILDRV
C            SILSTK
C            SIMSTK
C            SISTRI
C            SICKON
C            SIPOSK    ENTRY IN KONST
C            SINEGK    ENTRY IN CONST
C            OPER2
C
C         AUTHOR.   C.E.VANDONI          DATE 03/12/73
C
C...  PAW VERSION  ...  MAY 1988
C
C    ..................................................
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
C
      CHARACTER CL*8
      DIMENSION LL(6)
C
C
C
C
C   IS KLASS VALUE WITHIN THE BOUNDS OF COMPUTED GOTO BELOW
      IF(KLASS.LT.1.OR.KLASS.GT.40) GOTO 998
C
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23
     1,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40), KLASS
C
 998  CALL SINERR(6)
      GOTO 999
C
 1    CONTINUE
C
C  1                           STOP
C........ 01     ............
C
 2    CONTINUE
C
C  2                           ASSIGNEMENT STATEMENT
C........ 02     ............
C
      GO TO 999
 3    CONTINUE
      CALL SIASSI
      IF(IERRNO.NE.0)RETURN
C
C  3                           ASSIGN SIMPLE NAME
C........ 03     ............
C
      GO TO 999
C  4,N-DIMENSION               INDEX,N-DIMENSIONS
 4    CONTINUE
C
C........ 04     ............
      CALL SINDLF
C
      GO TO 999
 5    CONTINUE
C
C  5                           ASSIGN ARRAY OR FUNCTION
C........ 05     ............
      CALL SINDXI
C
      GO TO 999
 6    CONTINUE
C
C    CREATE A TEMPORARY, TEMPORARY NAME IN STACK, VALUE IN VALUE AREA
C
      CALL SIKONS
C
C  6,CONSTANT                  CONSTANT
C........ 06     ............
C
      GO TO 999
 7    CONTINUE
C
C    VARIABLE NAME IN STACK
C
      CALL SIVARI
      IF(IERRNO.NE.0)RETURN
C
C  7,VARIABLE                  VARIABLE
C........ 07     ............
C
      GO TO 999
 8    CONTINUE
C
C 08                           **  OR  '
C........ 08     ............
C    ALL NOW GO TO 8999
C
 9    CONTINUE
C
C 09                           *
C........ 09     ............
C
 10   CONTINUE
C
C 10                           /
C........ 10     ............
C
 11   CONTINUE
C
C 11                           -
C........ 11     ............
C
 12   CONTINUE
C
C 12                           +
C........ 12     ............
C
 13   CONTINUE
C
C 13                           EQ   EQUALS TO
C........ 13     ............
C
 14   CONTINUE
C
C 14                           NE   NOT EQUALS
C........ 14     ............
C
 15   CONTINUE
C
C 15                           GT   GREATHER THAN
C........ 15     ............
C
 16   CONTINUE
C
C 16                           LT   LESS THAN
C........ 16     ............
C
 17   CONTINUE
C
C 17                           GE   GREATHER OR EQUAL
C........ 17     ............
C
 18   CONTINUE
C
C 18                           LE   LESS OR EQUAL
C........ 18     ............
C
      GO TO 8999
 19   CONTINUE
C
C 19                           ?    CONC
C........ 19     ............
C
      CALL SIOPR2
      IF(IERRNO.NE.0)RETURN
      GO TO 999
 20   CONTINUE
C
C 20                           &    AND
C........ 20     ............
C
 21   CONTINUE
C
C 21                           !    OR
C........ 21     ............
C
      GO TO 8999
 22   CONTINUE
C
C 22,FUNCTION-NO.    SIGMA-SYS-FUNCTION CALL,NO OF FUNCTION
C    IF FUNCTION-NO EQUALS 1 THEN IT IS AN ARRAY DECLARATION - SEE 200.
C........ 22     ............
C
C    ALSO PROCESSING KLASS 25 SYSFUNS WITH VARIABLE NUMBER OF PARAMETERS
C    ALSO PROCESSING KLASS 37 LOGICAL NOT
C
C   IF LOGICAL NOT, SYSFUN CODE ALREADY IN CLASS
      IF(KLASS.NE.37) CALL SINEXT(KLASS)
C
C... !!! KLASS HOLDS NOW
C...                     INTERNAL SYSFUN CODE!!!
C
      IF(KLASS.GT.600) GOTO 2201
C     MUST HAVE A SYSTEM FUNCTION
      CALL SISYSF
      IF(IERRNO.NE.0)RETURN
      GOTO 999
C
 2201 CONTINUE
C      MUST HAVE SYSTEM SIGMA OPERATOR
      CALL SISYSO
      IF(IERRNO.NE.0)RETURN
      GOTO 999
C
 23   CONTINUE
C
C 23,NAME                      PROGRAMMERS FUNCTION CALL,NAME
C........ 23     ............
C
      CALL SILSKK(0,LL,CL)
      DO 3333 I=1,ITAM
      INAME(I)=LL(I)
 3333 CONTINUE
      CNAME=CL
      CALL SIGECD
      IF(ITYPE.EQ.5) GO TO 2378
      IF(ITYPE.EQ.3)GO TO 4111
      IF(ITYPE.EQ.17) GOTO 997
      CALL SINDXI
      GO TO 999
997   CALL SINERR(7)
      GOTO 999
C
4111  CONTINUE
      CALL SINEXT(N)
CEV   CALL SIACTU(N)
      GO TO 999
C
C
 2378 CONTINUE
      CALL SILDRV
      IF(IERRNO.NE.0)RETURN
C
      GO TO 999
C
C
 24   CONTINUE
C
C 24                           EMPTY INDEX EXPRESSION
C........ 24     ............
C
C   PUT A ITYPE = 6 IN STACK FOR MISSING INDEX
C
      DO 3334 I=1,ITAM
      LL(I)=0
 3334 CONTINUE
      LL(1)=6
      CL='        '
      MN=6
      MP=6
C     MISSING INDEX
      CALL SILSTK(LL,CL)
      CALL SIMSTK(MP,MN)
      GO TO 999
 25   CONTINUE
C
C 25  IS COMPILED FOR SYSFUNS WITH A VARIABLE NUMBER OF PARAMETERS
C........ 25     ............
      GOTO 22
C
C........ 28     ............
C
 26   CONTINUE
C
C
C 26,N-CHARS,STRING            STRING HAVING N CHARACTERS
C........ 26     ............
C
      CALL SISTRI
      GO TO 999
 27   CONTINUE
C
C 27,VARIABLE                  PARAMETRIC NAME,VARIABLE(VARIABLE)
C........ 27     ............
C
      GO TO 999
 28   CONTINUE
C
C 28                 BOS  BEGINNING OF STATEMENT
C
C
C     NOW READY FOR TEMPORARIES CREATION
         ISTRI=0
C
      GO TO 999
 29   CONTINUE
C
C 29                 EOS  END OF STATEMENT
C........ 29     ............
      CALL SIEOST
C
      GO TO 999
 30   CONTINUE
C
C........ 30     ............
C
C         FREE
      GO TO 999
C
 31   CONTINUE
C
C 31                            COMPLEX CONSTANTS
C........ 31     ............
      CALL SICKON
C
      GO TO 999
 32   CONTINUE
C
C........ 32     ............
C
C
      CALL SIPOSK
      GO TO 999
C
 33   CONTINUE
C
C........ 33     ............
      CALL SINEGK
      GO TO 999
 34   CONTINUE
C
C........ 34     ............
C 34                                  TRACK XXX
      CALL SINEXT(I)
      SITRAK(I)=1
      GO TO 999
 35   CONTINUE
C
C 35                                  !NOTRACK
C........ 35     ............
      DO 3335 J=1,60
      SITRAK(J)=0
 3335 CONTINUE
      GO TO 999
 36   CONTINUE
C
C 36                           MULT   (FOR MATRIX MULTIPLICATION)
C........ 36     ............
      CALL SINERR(54)
C
      GO TO 999
 37   CONTINUE
C
C 37                           NOT   LOGICAL NOT
C........ 37     ............
C
      GOTO 22
 38   CONTINUE
C
C******** 46     *******
C    ALL NOW GO TO 8999     OR TO 999
 39   CONTINUE
C
C******** 47     *******
      GO TO 999
 40   CONTINUE
C
C******** 50     *******
C
8999   CONTINUE
      CALL SIOPR2
      IF(IERRNO.NE.0)RETURN
      GO TO 999
C
 999  END
