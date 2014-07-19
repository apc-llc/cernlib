*
* $Id: silif1.F,v 1.3 1998/03/06 09:02:39 couet Exp $
*
* $Log: silif1.F,v $
* Revision 1.3  1998/03/06 09:02:39  couet
* - ATG is now disabled
*
* Revision 1.2  1997/03/14 11:57:32  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:35:43  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
         SUBROUTINE SILIF1(A,Y)
C
C
C   .......................................................
C
C
C     LIBRARY FUNCTIONS WITH ONE OPERAND.
C     -----------------------------------
C       LIBRARY FUNCTIONS FOR REAL ARGUMENTS
C       ------------------------------------
C
C 001    ACOS           COMPILER FUNCTION
C
C 002    ASIN           COMPILER FUNCTION
C
C 003    TAN            COMPILER FUNCTION
C
C 004    SINH           (B200)
C
C 005    COSH           (B200)
C
C 006    ERF            (C300)
C
C 007    ERFC           (C300)
C
C 008    FREQ           (C300)
C
C 009    DILOG          (C304)
C
C 010    GAMMA          (C305)
C
C 011    ELLICK         (C308)
C
C 012    ELLICE         (C308)
C
C 013    BESJ0          (C312)
C
C 014    BESJ1          (C312)
C
C 015    BESY0          (C312)
C
C 016    BESY1          (C312)
C
C 017    BESI0          (C313)
C
C 018    BESI1          (C313)
C
C 019    BESK0          (C313)
C
C 020    BESK1          (C313)
C
C 021    EBESI0         (C313)
C
C 022    EBESI1         (C313)
C
C 023    EBESK0         (C313)
C
C 024    EBESK1         (C313)
C
C 025    SININT         (C336)
C
C 026    COSINT         (C336)
C
C 027    EXPINT         (C337)
C
C 028    DAWSON         (C339)
C
C 029    ALOGAM         (C341)
C
C 030    RNDM           COMPILER FUNCTION
C
C...PAW VERSION  ...  MAY 1988
C
C   ..........................................................
C
#include "sigma/sigc.inc"
C
C
C     CALL SITRAC(' SILIF1   ')
C
C
C
      ACONST=741.66
      UPSI=2.0E14
C
      Y=0.
      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     C23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,
     C44,45),KLASS
C
C
   1   CONTINUE
C
C 001    ACOS           COMPILER FUNCTION
C-----------------------------
C
C      CALL SITRAC(' ACOS    ')
C
      IF(ABS(A) .GT.1.0) GO TO 200
C
       Y=ACOS   (A)
       GO TO 999
C
C
   2   CONTINUE
C
C 002    ASIN           COMPILER FUNCTION
C-----------------------------
C
C      CALL SITRAC(' ASIN    ')
C
      IF(ABS(A) .GT.1.0) GO TO 200
C
       Y=ASIN   (A)
       GO TO 999
C
C
   3   CONTINUE
C
C 003    TAN            COMPILER FUNCTION
C-----------------------------
C
C      CALL SITRAC(' TAN     ')
C
      IF(ABS(A) .GT.8.4E14) GO TO 200
C
       Y=TAN    (A)
       GO TO 999
C
C
   4   CONTINUE
C
C 004    SINH           (B200)
C-----------------------------
C
C      CALL SITRAC(' SINH    ')
C
      IF(A.GT.ACONST) GO TO 200
C
       Y=SINH   (A)
       GO TO 999
C
C
   5   CONTINUE
C
C 005    COSH           (B200)
C-----------------------------
C
C      CALL SITRAC(' COSH    ')
C
      IF(A.GT.ACONST) GO TO 200
C
       Y=COSH   (A)
       GO TO 999
C
C
   6   CONTINUE
C
C 006    ERF            (C300)
C-----------------------------
C
C      CALL SITRAC(' ERF     ')
C
       Y=ERF    (A)
       GO TO 999
C
C
   7   CONTINUE
C
C 007    ERFC           (C300)
C-----------------------------
C
C      CALL SITRAC(' ERFC    ')
C
       Y=ERFC   (A)
       GO TO 999
C
C
   8   CONTINUE
C
C 008    FREQ           (C300)
C-----------------------------
C
C      CALL SITRAC(' FREQ    ')
C
       Y=FREQ   (A)
       GO TO 999
C
C
   9   CONTINUE
C
C 009    DILOG          (C304)
C-----------------------------
C
C     CALL SITRAC(' DILOG   ')
C
C    DUMMY CALL TO DILOG
C
        Y=DILOG  (A)
C

       GO TO 999
C
C
  10   CONTINUE
C
C 010    GAMMA          (C305)
C-----------------------------
C
C      CALL SITRAC('GAMMA    ')
      IF(A.LE.0.0) GO TO 200
C
       Y=GAMMA  (A)
       GO TO 999
C
C
  11   CONTINUE
C
C 011    ELLICK         (C308)
C-----------------------------
C
C      CALL SITRAC('ELLICK   ')
C
       Y=ELLICK (A)
       GO TO 999
C
C
  12   CONTINUE
C
C 012    ELLICE         (C308)
C-----------------------------
C
C      CALL SITRAC('ELLICE   ')
C
       Y=ELLICE (A)
       GO TO 999
C
C
  13   CONTINUE
C
C 013    BESJ0          (C312)
C-----------------------------
C
C      CALL SITRAC('BESJ0    ')
C
      IF(A.GT.UPSI) GO TO 200
C
       Y=BESJ0  (A)
       GO TO 999
C
C
  14   CONTINUE
C
C 014    BESJ1          (C312)
C-----------------------------
C
C      CALL SITRAC('BESJ1    ')
C
      IF(A.GT.UPSI) GO TO 200
C
       Y=BESJ1  (A)
       GO TO 999
C
C
  15   CONTINUE
C
C 015    BESY0          (C312)
C-----------------------------
C
C      CALL SITRAC('BESY0    ')
C
      IF(A.LE.0.0) GO TO 200
      IF(A.GT.UPSI) GO TO 200
C
       Y=BESY0  (A)
       GO TO 999
C
C
  16   CONTINUE
C
C 016    BESY1          (C312)
C-----------------------------
C
C      CALL SITRAC('BESY1    ')
C
      IF(A.LE.0.0) GO TO 200
      IF(A.GT.UPSI) GO TO 200
C
       Y=BESY1  (A)
       GO TO 999
C
C
  17   CONTINUE
C
C 017    BESI0          (C313)
C-----------------------------
C
C      CALL SITRAC('BESI0    ')
C
      IF(A.GT.ACONST) GO TO 200
C
       Y=BESI0  (A)
       GO TO 999
C
C
  18   CONTINUE
C
C 018    BESI1          (C313)
C-----------------------------
C
C      CALL SITRAC('BESI1    ')
C
      IF(A.GT.ACONST) GO TO 200
C
       Y=BESI1  (A)
       GO TO 999
C
C
  19   CONTINUE
C
C 019    BESK0          (C313)
C-----------------------------
C
C      CALL SITRAC('BESK0    ')
C
      IF(A.LE.0.0) GO TO 200
      IF(A.GT.ACONST) GO TO 200
C
       Y=BESK0  (A)
       GO TO 999
C
C
  20   CONTINUE
C
C 020    BESK1          (C313)
C-----------------------------
C
C      CALL SITRAC('BESK1    ')
C
      IF(A.LE.0.0) GO TO 200
      IF(A.GT.ACONST) GO TO 200
C
       Y=BESK1  (A)
       GO TO 999
C
C
  21   CONTINUE
C
C 021    EBESI0         (C313)
C-----------------------------
C
C      CALL SITRAC('EBESI0   ')
C
       Y=EBESI0 (A)
       GO TO 999
C
C
  22   CONTINUE
C
C 022    EBESI1         (C313)
C-----------------------------
C
C      CALL SITRAC('EBESI1   ')
C
       Y=EBESI1 (A)
       GO TO 999
C
C
  23   CONTINUE
C
C 023    EBESK0         (C313)
C-----------------------------
C
C      CALL SITRAC('EBESK0   ')
C
      IF(A.LE.0.0) GO TO 200
C
       Y=EBESK0 (A)
       GO TO 999
C
C
  24   CONTINUE
C
C 024    EBESK1         (C313)
C-----------------------------
C
C      CALL SITRAC('EBESK1   ')
C
      IF(A.LE.0.0) GO TO 200
C
       Y=EBESK1 (A)
       GO TO 999
  25   CONTINUE
C
C 025    SININT         (C336)
C-----------------------------
C
C      CALL SITRAC('SININT   ')
C
       Y=SININT (A)
       GO TO 999
C
C
  26   CONTINUE
C
C 026    COSINT         (C336)
C-----------------------------
C
C      CALL SITRAC('COSINT   ')
C
      IF(A.EQ.0.0) GO TO 200
C
       Y=COSINT (A)
       GO TO 999
C
C
  27   CONTINUE
C
C 027    EXPINT         (C337)
C-----------------------------
C
C      CALL SITRAC('EXPINT   ')
C
       Y=EXPINT (A)
       GO TO 999
C
C
C
  28   CONTINUE
C
C 028    DAWSON         (C339)
C-----------------------------
C
C      CALL SITRAC(' DAWSON  ')
C
C
C
        Y=DAWSON (A)
C
C
       GO TO 999
C
C
  29   CONTINUE
C
C 029    ALOGAM         (C341)
C-----------------------------
C
C      CALL SITRAC(' ALOGAM  ')
C
      IF(A.LE.0.0) GO TO 200
C
       Y=ALOGAM (A)
       GO TO 999
C
C
  30   CONTINUE
C
C 030    RNDM           COMPILER FUNCTION
C-----------------------------
C
C      CALL SITRAC(' RNDM    ')
C
       Y=RNDM   (A)
       GO TO 999
C
  31   CONTINUE
  32   CONTINUE
  33   CONTINUE
  34   CONTINUE
       GOTO 200
  35   CONTINUE
       IF(A.EQ.0.0) GO TO 200
       IF((A.LT.0.0).AND.(ABS(INT(A)).EQ.ABS(A))) GO TO 200
       Y=ADIGAM(A)
       GOTO 999
  36   CONTINUE
       Y=ASINH(A)
       GOTO 999
  37   CONTINUE
       Y=ATANI(A)
       GOTO 999
  38   CONTINUE
C      Y = ATG(A)
       CALL SINERR(9)
       GOTO 999
  39   CONTINUE
       Y=FRSIN(A)
       GOTO 999
  40   CONTINUE
       Y=FRCOS(A)
       GOTO 999
  41   CONTINUE
       IF(A.EQ.0.0) GO TO 200
       IF((A.LT.0.0).AND.(ABS(INT(A)).EQ.ABS(A))) GO TO 200
       Y=GAMMF(A)
       GOTO 999
  42   CONTINUE
       IF(A.LE.0.0.or.A.GE.1.0)GOTO 200
       Y=GAUSIN(A)
       GOTO 999
  43   CONTINUE
       Y=RANGAM(A)
       GOTO 999
  44   CONTINUE
       Y=STRH0(A)
       GO TO 999
  45   CONTINUE
       Y=STRH1(A)
       GOTO 999
 200   Y=0.
C
 999  END
