*
* $Id: concdc.F,v 1.1.1.1 1996/02/15 17:47:37 mclareni Exp $
*
* $Log: concdc.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:37  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE CONCDC(DWORD,N,SWORD,IERR,LERR)
C
C     Authors: Eric McINTOSH - Elia PEROTTO                    Sept.1986
C
C     This routine tries to convert CDC format single precision integers
C     and floating point numbers to IBM single precision.
C     The input data is assumed to be stored 60 bits per 64-bit IBM word
C     (right justified) and the 32-bit results are stored in SWORD which
C     may replace the input.
C
C     DWORD : the REAL*8 input array of 60 in 64 bit data
C     N     : the number of data items to be converted
C     SWORD : the single precision 32-bit array of results
C     IERR  : will be set non-zero if at least one error was detected
C     LERR  : if .LT. zero no errors are listed,
C             if .EQ. zero errors are listed on standard output,
C             if .GT. zero on logical unit LERR.
C
      DIMENSION SWORD (*)
      CHARACTER*10 A10
      REAL*8 DWORD (*) , WORD, RWORD
      INTEGER IWORD(2) , JWORD(2), KWORD(2)
      EQUIVALENCE (IWORD(1),WORD), (KWORD(1),RWORD), (LWORD,TWORD)
      LOGICAL INT,ERROR
      DATA M1/Z00000001/, M12/Z00000FFF/
     1    M28/Z0FFFFFFF/, M31/Z7FFFFFFF/, M32/ZFFFFFFFF/
C
C     IERR reports at least one special case occurence if non-zero
C     special cases are listed on standard output (LERR = 0) or on
C     unit LERR (LERR > 0) or ignored (LERR < 0).
C
      IERR = 0
      DO 1 I=1,N
      ERROR = .FALSE.
      INT = .FALSE.
C                       for debugging
C     IERR = 99
C     ERROR = .TRUE.
C
      WORD = DWORD (I)
      ISIGN = IAND( ISHFT( IWORD(1),-27 ) , M1)
      IF ( ISIGN .NE. 0 ) THEN
         JWORD (1) = IEOR ( IWORD(1),M28)
         JWORD (2) = IEOR ( IWORD(2),M32 )
      ELSE
         JWORD(1) = IWORD (1)
         JWORD(2) = IWORD (2)
      ENDIF
      IEXP = IAND ( ISHFT (JWORD(1),-16) , M12)
      RWORD = DWORD(I)
      IF (JWORD(1) .EQ. 0 ) THEN
C        we can assume an integer (almost)
         INT = .TRUE.
         ISIGN2 = IAND (ISHFT (JWORD(2),-31) , M1)
         IF (ISIGN2 .NE. 0 .AND. ISIGN .EQ. 0) THEN
            IERR = 11
            ERROR = .TRUE.
C           this is a too big positive integer or underflow
            INT = .FALSE.
         ENDIF
      ELSE
C        we have to assume floating point even if integer is too big ???
         IF (IEXP .EQ. 2047) THEN
C           infinite value
            IERR = 12
            ERROR = .TRUE.
         ELSEIF (IEXP .EQ. 1023) THEN
C           indefinite value
            IERR = 13
            ERROR = .TRUE.
         ELSEIF (IEXP .EQ. 0) THEN
C           underflow perhaps or a very big integer
            IERR = 11
            ERROR = .TRUE.
         ELSEIF (IEXP .GE. 1229 .OR. IEXP .LE.  715) THEN
C           not in IBM range
            IERR = 14
            ERROR = .TRUE.
         ENDIF
      ENDIF
      IF (ERROR) THEN
         CALL CDC$A4(DWORD(I),A10,1,10)
         IF (IERR .EQ. 11 .OR. IERR .EQ. 13) THEN
            KWORD(1) = 0
            KWORD(2) = 0
         ENDIF
      ENDIF
      CALL CVT76A (RWORD, 1)
      IF (INT) THEN
         LWORD = KWORD (2)
         SWORD(I) = TWORD
      ELSE
         SWORD(I) = SNGL(RWORD)
      ENDIF
      IF (ERROR) THEN
         IF (LERR .EQ. 0) THEN
            WRITE (*,100) I,IERR, WORD, ISIGN, IEXP, IEXP, A10,
     1                    RWORD, RWORD, KWORD(1), KWORD (2),
     2                    SWORD(I), SWORD(I), KWORD(2)
         ELSEIF (LERR .GT. 0) THEN
            WRITE (LERR,100) I,IERR, WORD, ISIGN, IEXP, IEXP, A10,
     1                    RWORD, RWORD, KWORD(1), KWORD (2),
     2                    SWORD(I), SWORD(I), KWORD(2)
         ENDIF
      ENDIF
 1    CONTINUE
      RETURN
C
 100  FORMAT(/,' ITEM NO ',I10,
     1 /,' CONCDC ERROR DETECTED:   IERR = ',I2,
     2   '   (11=BIG INTEGER/UNDERFLOW, 12=INF, 13=IND, 14=RANGE)',
     3 /,' INPUT = ',Z16,'    SIGN = ',I1,'    EXP = ',I4,' #',Z3,
     +   '     CHAR = ''',A10,'''',
     4 /,' VALUE = ',Z16,E25.15,I15,I15,
     5 /,' SNGLE = ',Z8,8X,E17.7,8X,I15)
      END
