*
* $Id: swords.F,v 1.10 2001/10/08 14:03:18 mclareni Exp $
*
* $Log: swords.F,v $
* Revision 1.10  2001/10/08 14:03:18  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE SWORDS (STRING,I,J,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find position of first and last non-blank
C-                         and non-tab characters in specified string.
C-                         This routine is a useful for removing
C-                         trailing and leading blanks.
C-
C-   Inputs  : STRING      String to be searched
C-
C-   Outputs : I           Start of "words"
C-             J           End of "words"
C-             N           Length of "words"
C-
C-   Created   9-MAR-1988   Harrison B. Prosper
C-   Updated  20-DEC-1988   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       I,J,K,L,M,N,LSTR
      CHARACTER*(*) STRING
C
      CHARACTER*1 SPACE,TAB
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
      CALL WORD (STRING(1:LSTR),I,J,N)
C
C ****  If string contains non-blank characters do a back-search
C
      IF ( N .GT. 0 ) THEN
        DO 10 K = LSTR,I,-1
          IF ( STRING(K:K) .NE. SPACE .AND.
     &         STRING(K:K) .NE. TAB ) THEN
            J = K
            GOTO 20
          ENDIF
   10   CONTINUE
      ENDIF
   20 CONTINUE
      N = J-I+1
C
  999 RETURN
      END
