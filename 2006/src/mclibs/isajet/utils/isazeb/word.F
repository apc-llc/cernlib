*
* $Id: word.F,v 1.10 2001/10/08 14:03:18 mclareni Exp $
*
* $Log: word.F,v $
* Revision 1.10  2001/10/08 14:03:18  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE WORD (STRING,I,J,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find first sub-string bounded by blanks or
C-                         tabs from the specified string, and return
C-                         the start and end position of the sub-string.
C-                         This routine is useful for splitting a string
C-                         into "words". See also SWORDS.
C-
C-   Inputs  : STRING      String to be searched
C-
C-   Outputs : I           Start of "word"
C-             J           End of "word"
C-             N           Length of "word"
C-
C-   Created   9-MAR-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER       I,J,N,K,LSTR
C
      CHARACTER*1 TAB,SPACE
      PARAMETER( SPACE = ' ' )
C----------------------------------------------------------------------
C
#if defined(CERNLIB_IBM)
C          Not ASCII
      TAB=' '
#endif
#if !defined(CERNLIB_IBM)
C          ASCII
      TAB=CHAR(9)
#endif
      I = 1
      J = 0
      N = 0
      LSTR = LEN (STRING)
C
      DO 10 K = 1,LSTR
        IF ( STRING(K:K) .NE. SPACE .AND.
     &       STRING(K:K) .NE. TAB ) THEN
          I = K
          N = INDEX (STRING(K:LSTR),SPACE) - 1
          IF ( N .LE. 0 ) THEN
            N = INDEX (STRING(K:LSTR),TAB) - 1
            IF ( N .LE. 0 ) THEN
              N = LSTR - I + 1
            ENDIF
          ENDIF
          J = I + N - 1
          GOTO 999
        ENDIF
   10 CONTINUE
C
  999 RETURN
      END
