*
* $Id: ffcard.F,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: ffcard.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
#if defined(CERNLIB_MACMPW)
!!S SEG_FFCARD
#endif
      SUBROUTINE FFCARD (BUFFER, KURIN, ISYMB, KUROUT, LENGTH, IVALUE)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine scans the string given in BUFFER for
C              tokens. These are integer or floating point numbers
C              or alphanumeric symbols.
C
C      DUMMY ARGUMENTS:
C
C              BUFFER - passed length character string containing
C                       line to be parsed.
C              KURIN  - position in BUFFER to start scan
C              ISYMB  - type of token found. Values are:
C                        1 - text (IVALUE undefined)
C                        2 - a special character (IVALUE undefined)
C                        3 - an integer number
C                        4 - a floating point number
C                        5 - a logical value - allowed are:
C                              T, TRUE,    and ON  for .TRUE.
C                              F, FALS(E), and OFF for .FALSE.
C                        6 - no token found up to end of string.
C                            In this case, the other output
C                            parameters are undefined.
C              KUROUT - position in BUFFER token starts
C              LENGTH - length of token found
C              IVALUE - value associated with token if ISYMB = 3, 4, 5
C
C      IMPLICIT INPUTS:
C
C              NONE
C
C      IMPLICIT OUTPUTS:
C
C              NONE
C
C      SIDE EFFECTS:
C
C              NONE
C
#include "ffread/cfread.inc"
      CHARACTER BUFFER*(*)
 
      CHARACTER*(*) ALPHA, DIGITS, SPECAL
      PARAMETER (SPECAL = '*''=$()"/')
      PARAMETER (DIGITS = '1234567890')
#if defined(CERNLIB_UPLOW)
      PARAMETER (ALPHA =
     * 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')
 
      CHARACTER CHECK*5
#endif
#if !defined(CERNLIB_UPLOW)
      PARAMETER (ALPHA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
#endif
 
      LOGICAL LVALU
      EQUIVALENCE (IVALU, RVALU, LVALU)
C
C----------------- Beginning of executable statements -------------------------
C
C        Reset some variables to default values
C
      ISYMB  = 0
      IVALU  = 0
      IEXP   = 0
      ISGNVA = 1
      ISGNEX = 1
      MANTIS = 0
      MANLEN = 0
 
      ICUR   = KURIN - 1
      IMAX   = LEN (BUFFER)
C
C        Loop over all characters remaining
C
 100  ICUR   = ICUR + 1
      IF (ICUR .GT. IMAX) GO TO 200
      GO TO (1, 11, 201, 31, 41), ISYMB + 1
      GO TO 201
C
C        We have no idea what to expect - so throw blanks etc. away
C
   1  IF (BUFFER(ICUR:ICUR) .EQ. ' '
#if defined(CERNLIB_TABS)
     *  .OR. ICHAR (BUFFER(ICUR:ICUR)) .EQ. 9
#endif
     *  ) GO TO 100
C
C        Check for a digit or decimal point
C
#if !defined(CERNLIB_VAX)
      ITMP1 = INDEX (DIGITS, BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
      ITMP1 = LIB$LOCC (BUFFER(ICUR:ICUR), DIGITS)
#endif
 
      IF (ITMP1 .NE. 0) THEN
          ISYMB  = 3
          KUROUT = ICUR
          LENGTH = 1
          IF (ITMP1 .NE. 10) IVALU = ITMP1
          GO TO 100
      END IF
 
      IF (BUFFER(ICUR:ICUR) .EQ. '.') THEN
          ISYMB  = 4
          KUROUT = ICUR
          LENGTH = 1
          IFIELD = 1
          GO TO 100
      END IF
C
C        Check for unary + or -
C
      IF (BUFFER(ICUR:ICUR) .EQ. '-') THEN
          ISGNVA = -1
          KUROUT = ICUR
          LENGTH = 1
          ISYMB  =  3
          GO TO 100
      END IF
 
      IF (BUFFER(ICUR:ICUR) .EQ. '+') THEN
          ISYMB  =  3
          KUROUT = ICUR
          LENGTH = 1
          GO TO 100
      END IF
C
C        Check for text and special characters
C
#if !defined(CERNLIB_VAX)
      ITMP2 = INDEX (ALPHA,  BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
      ITMP2 = LIB$LOCC (BUFFER(ICUR:ICUR), ALPHA)
#endif
      IF (ITMP2 .NE. 0) GO TO 10
 
#if !defined(CERNLIB_VAX)
      ITMP2 = INDEX (SPECAL, BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
      ITMP2 = LIB$LOCC (BUFFER(ICUR:ICUR), SPECAL)
#endif
      IF (ITMP2 .NE. 0) GO TO 20
C
C        Whatever this might be - we'll ignore it
C
      GO TO 100
C
C        Start up a text
C
  10  ISYMB  = 1
      KUROUT = ICUR
      LENGTH = 1
      GO TO 100
C
C        Continue with text.
C        If we find something else, check for logical constants.
C
#if !defined(CERNLIB_VAX)
  11    ITMP3 = INDEX (ALPHA,  BUFFER(ICUR:ICUR))
      IF (ITMP3 .EQ. 0)
     *  ITMP3 = INDEX (DIGITS, BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
  11    ITMP3 = LIB$LOCC (BUFFER(ICUR:ICUR), ALPHA)
      IF (ITMP3 .EQ. 0)
     *  ITMP3 = LIB$LOCC (BUFFER(ICUR:ICUR), DIGITS)
#endif
 
      IF (ITMP3 .NE. 0) THEN
          LENGTH = LENGTH + 1
          GO TO 100
      END IF
 
      GO TO 210
C
C
C        Special character - that's easy
C
  20  ISYMB  = 2
      KUROUT = ICUR
      LENGTH = 1
      RETURN
C
C        Digit of integer; if decimal point, switch to real
C
#if !defined(CERNLIB_VAX)
  31  ITMP4 = INDEX (DIGITS, BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
  31  ITMP4 = LIB$LOCC (BUFFER(ICUR:ICUR), DIGITS)
#endif
 
      IF (ITMP4 .NE. 0) THEN
          LENGTH = LENGTH + 1
          IVALU  = IVALU * 10
          IF (ITMP4 .NE. 10) IVALU = IVALU + ITMP4
          GO TO 100
      END IF
 
      IF (BUFFER(ICUR:ICUR) .EQ. '.') THEN
          ISYMB  = 4
          LENGTH = LENGTH + 1
          IFIELD = 1
          GO TO 100
      END IF
 
#if defined(CERNLIB_UPLOW)
      IF (   BUFFER(ICUR:ICUR) .EQ. 'E'
     *  .OR. BUFFER(ICUR:ICUR) .EQ. 'e') THEN
          CALL FFUPCA (BUFFER, ICUR, 1)
#endif
#if !defined(CERNLIB_UPLOW)
      IF (   BUFFER(ICUR:ICUR) .EQ. 'E') THEN
#endif
          ISYMB  = 4
          LENGTH = LENGTH + 1
          IFIELD = 2
          GO TO 100
      END IF
 
      GO TO 230
C
C        Real number: dispatch on current field
C
  41  GO TO (46, 47, 48), IFIELD
C
C        Scanning fractional part - expect digit, 'E', or end
C
#if !defined(CERNLIB_VAX)
  46  ITMP5 = INDEX (DIGITS, BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
  46  ITMP5 = LIB$LOCC (BUFFER(ICUR:ICUR), DIGITS)
#endif
 
      IF (ITMP5 .NE. 0) THEN
          LENGTH = LENGTH + 1
          MANLEN = MANLEN + 1
          MANTIS = MANTIS * 10
          IF (ITMP5 .NE. 10) MANTIS = MANTIS + ITMP5
          GO TO 100
      END IF
 
#if defined(CERNLIB_UPLOW)
      IF (   BUFFER(ICUR:ICUR) .EQ. 'E'
     *  .OR. BUFFER(ICUR:ICUR) .EQ. 'e') THEN
          CALL FFUPCA (BUFFER, ICUR, 1)
#endif
#if !defined(CERNLIB_UPLOW)
      IF (   BUFFER(ICUR:ICUR) .EQ. 'E') THEN
#endif
          LENGTH = LENGTH + 1
          IFIELD = 2
          GO TO 100
      END IF
 
      GO TO 240
C
C        Last item was 'E'. Check for exponent sign
C
  47  IFIELD = 3
 
      IF (BUFFER(ICUR:ICUR) .EQ. '-') THEN
          ISGNEX = -1
          LENGTH = LENGTH + 1
          GO TO 100
      END IF
 
      IF (BUFFER(ICUR:ICUR) .EQ. '+') THEN
          LENGTH = LENGTH + 1
          GO TO 100
      END IF
C
C        Scanning exponent field
C
#if !defined(CERNLIB_VAX)
  48  ITMP6 = INDEX (DIGITS, BUFFER(ICUR:ICUR))
#endif
#if defined(CERNLIB_VAX)
  48  ITMP6 = LIB$LOCC (BUFFER(ICUR:ICUR), DIGITS)
#endif
 
      IF (ITMP6 .NE. 0) THEN
          LENGTH = LENGTH + 1
          IFIELD = 3
          IEXP   = IEXP * 10
          IF (ITMP6 .NE. 10) IEXP = IEXP + ITMP6
          GO TO 100
      END IF
 
      GO TO 240
C
C        We get here when the string is exhausted.
C        If ISYMB = 0, no token found.
C        If not, finish current token.
C
 200  IF (ISYMB .EQ. 0) THEN
          ISYMB  = 6
          RETURN
      END IF
 
      GO TO (210, 201, 230, 240), ISYMB
 
 201  WRITE(LUNOUT,'('' FFCARD INTERNAL CODING ERROR'')')
      STOP
C
C        For text, check whether it's a logical constant
C
 210  IF (LENGTH .GT. 5) RETURN
#if defined(CERNLIB_UPLOW)
      CHECK(1:LENGTH) = BUFFER(KUROUT:KUROUT+LENGTH-1)
      CALL FFUPCA (CHECK, 1, LENGTH)
#endif
      GO TO (211, 212, 213, 214, 215), LENGTH
      GO TO 201
 
#if defined(CERNLIB_UPLOW)
 211  IF (CHECK(1:1)  .EQ. 'T') GO TO 221
      IF (CHECK(1:1)  .EQ. 'F') GO TO 222
#endif
#if !defined(CERNLIB_UPLOW)
 211  IF (BUFFER(KUROUT:KUROUT) .EQ. 'T') GO TO 221
      IF (BUFFER(KUROUT:KUROUT) .EQ. 'F') GO TO 222
#endif
      RETURN
 
#if defined(CERNLIB_UPLOW)
 212  IF (CHECK(1:2)  .EQ. 'ON') GO TO 221
#endif
#if !defined(CERNLIB_UPLOW)
 212  IF (BUFFER(KUROUT:KUROUT+1) .EQ. 'ON') GO TO 221
#endif
      RETURN
 
#if defined(CERNLIB_UPLOW)
 213  IF (CHECK(1:3)  .EQ. 'OFF') GO TO 222
#endif
#if !defined(CERNLIB_UPLOW)
 213  IF (BUFFER(KUROUT:KUROUT+2) .EQ. 'OFF') GO TO 222
#endif
      RETURN
 
#if defined(CERNLIB_UPLOW)
 214  IF (CHECK(1:4)  .EQ. 'TRUE') GO TO 221
      IF (CHECK(1:4)  .EQ. 'FALS') GO TO 222
#endif
#if !defined(CERNLIB_UPLOW)
 214  IF (BUFFER(KUROUT:KUROUT+3) .EQ. 'TRUE') GO TO 221
      IF (BUFFER(KUROUT:KUROUT+3) .EQ. 'FALS') GO TO 222
#endif
      RETURN
 
#if defined(CERNLIB_UPLOW)
 215  IF (CHECK(1:5)  .EQ. 'FALSE') GO TO 222
#endif
#if !defined(CERNLIB_UPLOW)
 215  IF (BUFFER(KUROUT:KUROUT+4) .EQ. 'FALSE') GO TO 222
#endif
      RETURN
C
C        Termination processing for logical constants
C
 221  LVALU  = .TRUE.
      GO TO 223
 
 222  LVALU  = .FALSE.
 
 223  ISYMB  = 5
#if defined(CERNLIB_UPLOW)
      BUFFER(KUROUT:KUROUT+LENGTH-1) = CHECK(1:LENGTH)
#endif
      IVALUE = IVALU
      RETURN
C
C        Termination processing for integers
C
 230  IVALUE = IVALU * ISGNVA
      RETURN
C
C        Termination processing for floating point
C
 240  RVALU  = REAL(IVALU)
 
      IF (MANLEN .NE. 0 .AND. MANTIS .NE. 0) THEN
          RVALU  = RVALU + REAL(MANTIS) * 10.**(-MANLEN)
      END IF
 
      IF (ISGNVA .LT. 0) RVALU  = - RVALU
      IF (IEXP   .NE. 0) RVALU  =   RVALU * 10.**(IEXP*ISGNEX)
 
      IVALUE = IVALU
      RETURN
 
      END
