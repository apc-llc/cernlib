*
* $Id: kudpar.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kudpar.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.66/07 05/06/91  17.46.17  by  Fons Rademakers
*-- Author :
      SUBROUTINE KUDPAR(STRING,IPAR,RPAR,CPAR,LENGTH,TYPE)
*
********************************************************************************
*
* Decode unknown parameter in STRING
* TYPE returns : 'I' (Integer in IPAR), 'R' (Real in RPAR),
*                'C' (Character in CPAR of length LENGTH),
*                ' ' (error case)
*
*     Input :
*
      CHARACTER*(*) STRING
*
*     Output :
*
      INTEGER IPAR
      REAL RPAR
      CHARACTER*(*) CPAR
      INTEGER LENGTH
      CHARACTER*(*) TYPE
*
********************************************************************************
*
#include "kuip/kcques.inc"
*     CHARACTER*80 CTEMP
      CHARACTER*15 CVALUE
*
      IF (STRING.EQ.' ') THEN
        LENGTH=0
        CPAR=' '
        TYPE='C'
        GO TO 999
      ENDIF

      LENGTH=LENOCC(STRING)
      IF (LENGTH.GT.15) GO TO 200
      DO 5 I=1,LENGTH
        IF (STRING(I:I).NE.' ') GO TO 8
5     CONTINUE
8     CVALUE=STRING(I:)
      LENGTH=LENOCC(CVALUE)

      DO 10 I=1,LENGTH
        IF ((CVALUE(I:I).NE.'.').AND.
     +      (CVALUE(I:I).NE.'+').AND.
     +      (CVALUE(I:I).NE.'-').AND.
     +      (CVALUE(I:I).NE.'E').AND.
     +      (CVALUE(I:I).NE.'e').AND.
     +      (CVALUE(I:I).LT.'0'.OR.CVALUE(I:I).GT.'9')) GO TO 200
10    CONTINUE

      DO 20 I=1,LENGTH
        IF (I .EQ. 1) THEN
           IF ((CVALUE(I:I).LT.'0' .OR.  CVALUE(I:I).GT.'9') .AND.
     +         (CVALUE(I:I).NE.'+' .AND. CVALUE(I:I).NE.'-')) GO TO 100
        ELSE
           IF (CVALUE(I:I).LT.'0'.OR.CVALUE(I:I).GT.'9') GO TO 100
        ENDIF
20    CONTINUE
*
* Case of Integer
*
      CALL KICTON(CVALUE,IPAR,RPAR)
      IF (IQUEST(1).NE.0.OR.IQUEST(2).NE.0) GO TO 200
      TYPE='I'
      GO TO 999
*
* Case of Real
*
100   CONTINUE
      CALL KICTON(CVALUE,IPAR,RPAR)
      IF (IQUEST(1).NE.0) GO TO 200
      TYPE='R'
      GO TO 999
*
* Case of Character
*
200   IQUEST(1)=0
      IF (LENGTH.GT.LEN(CPAR)) THEN
        CALL KUALFA
        PRINT *,'*** KUDPAR: The output character argument CPAR has ',
     +          'been defined with insufficient length =',LEN(CPAR)
        IQUEST(1)=1
        TYPE=' '
        GO TO 999
      ENDIF
      CPAR=' '
      CPAR=STRING
      TYPE='C'

999   END



