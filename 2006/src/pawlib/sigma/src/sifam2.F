*
* $Id: sifam2.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sifam2.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIFAM2
C
C    ....................................................
C         PURPOSE
C            PROCESS FAMILY 200 - ARRAYS
C
C         USAGE
C            CALL SIFAM2
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: N ONE
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAC
C            ARRAY
C            SINERR
C            SIDELX
C            SILIBR
C
C...  PAW VERSION  ...  MAY 1988
C
#include "sigma/sigc.inc"
C
C
C   IS KLASS VALUE WITHIN THE BOUNDS OF COMPUTED GOTO BELOW
      IF(KLASS.LE.0.OR.KLASS.GT.10)GOTO 997
      GO TO (1,2,3,4,5,6,207,210,211,212), KLASS
C
 997  CALL SINERR(6)
      GOTO 999
C
C
 1    CONTINUE
C
C
C201,N-DIMENSIONS              DIMENSIONS OF ARRAY DECLARE
C******** 201     ************
C
C
      GO TO 7
C
 2    CONTINUE
C
C
C202                           NO ARRAY VALUE DECLARE
C******** 202     ************
C
C    TYPE    A
C      PUT IN ALL ONES    NAME IS AT TOP OF STACK
C
C ARRAY(N&M&..) - FILLED WITH ONES
      CALL SIARRA(0)
C
C
      GO TO 7
C
 3    CONTINUE
C
C
C203                           ARRAY VALUE DECLARE
C******** 203     ************
C
C    TYPE    A
      GO TO 7
C
 4    CONTINUE
C
C
C204                           INTERVAL ARRAY VALUE DECLARE BEGIN
C******** 204     ************
C
      GO TO 7
C
 5    CONTINUE
C
C
C205                           INTERVAL ARRAY VALUE DECLARE END
C******** 205     ************
C ARRAY(N&M&..,A#B) - FILLED WITH NUMBERS IN AN INTERVAL
      CALL SIARRA(2)
C
      GO TO 7
C
 6    CONTINUE
C
C
C206,N-PARAMETERS              LIST OF ARRAY ELEMENT VALUES DECLARE,NO O
C******** 206     ************
C
C ARRAY(N&M&..,VALUE[S]) - FILLED WITH VALUE(S) PROVIDED
      CALL SIARRA(1)
      GO TO 7
207   CONTINUE
C !LIBRARY
C      CALL SILIBR
      GO TO 7
C
C
210   CONTINUE
C !DELETE
*      CALL SIDELX
      GO TO 7
C
C
211   CONTINUE
C !SIUSD
*      CALL SIUSD
      GO TO 7
C
C
212   CONTINUE
C !USERS
*      CALL SIUSRS
C
C
 7    CONTINUE
C
 999  END
