*
* $Id: ffkey.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffkey.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFKEY (KEY, ADRESS, LENGTH, CHTYPE)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine accepts the definition of a user key
C              for later use by FFGO.
C
C      DUMMY ARGUMENTS:
C
C              KEY    - character string containing the key to use
C              ADRESS - variable/array to store values to
C              LENGTH - number of values associated with this key
C              CHTYPE - character string containing either INTE, REAL,
C                       or LOGI to signify ADRESS contains integer,
C                       floating point, or logical data, respectively.
C
C      IMPLICIT INPUTS:
C
C              The currently defined keys are checked for unambiguity
C              of the new key. If no more keys can be defined, an error
C              message is issued.
C
C      IMPLICIT OUTPUTS:
C
C              CFREAD is updated to include the new information.
C
C      SIDE EFFECTS:
C
C              NONE
C
#include "ffread/ffmach.inc"
#include "ffread/ffluns.inc"
#include "ffread/cfread.inc"
      CHARACTER KEY*(*), CHTYPE*(*)
CF90  Declare ADRESS as integer to avoid consistency checking by LOCF
      INTEGER   ADRESS(1)
      INTEGER FFFIND
 
      CHARACTER KEYSTR*(NCHMAX)
C
C----------------- Beginning of executable statements -------------------------
C
C        Get key string into local variable, possibly truncating it
C
      KEYSTR(1:NCHKEY) = KEY
C
C        Check whether enough space for a new key
C
      IF (NKEYS .LT. MAXKEY) THEN
C
C        Check key for ambiguity (convert to upper case if necesssary).
C        This also gives us the place to insert it.
C
#if defined(CERNLIB_UPLOW)
          CALL FFUPCA (KEYSTR, 1, NCHKEY)
#endif
          IPOINT = FFFIND (KEYSTR)
 
          IF (IPOINT .GT. 0) THEN
              WRITE (UNIT=LUNOUT, FMT=1001) KEYSTR(1:NCHKEY)
              RETURN
          END IF
C
C        Check length is positive
C
          IF (LENGTH .LE. 0) THEN
              WRITE (UNIT=LUNOUT, FMT=1002) KEYSTR(1:NCHKEY)
              RETURN
          END IF
C
C        Find which type of key it is
C
          CALL UCTOH (CHTYPE, NTYPE, NCHWD, MIN (LEN (CHTYPE), 4) )
          ITYPE = IUCOMP (NTYPE, KEYOPT, 3)
C
C        Count this key and make room for it if necessary
C
          NKEYS  = NKEYS + 1
          IPOINT = - IPOINT
          ITEMP  =   NOFKEY + IPOINT*NWDKEY
 
          IF (IPOINT .NE. NKEYS) THEN
              CALL UCOPY2 (IWORDS(NOFLOC+IPOINT  ),
     *                     IWORDS(NOFLOC+IPOINT+1), NKEYS-IPOINT)
              CALL UCOPY2 (IWORDS(NOFLEN+IPOINT  ),
     *                     IWORDS(NOFLEN+IPOINT+1), NKEYS-IPOINT)
              CALL UCOPY2 (IWORDS(NOFTYP+IPOINT  ),
     *                     IWORDS(NOFTYP+IPOINT+1), NKEYS-IPOINT)
              CALL UCOPY2 (IWORDS(ITEMP),
     *                     IWORDS(ITEMP+NWDKEY), NWDKEY*(NKEYS-IPOINT))
          END IF
C
C        Now fill in data as appropriate
C
          IWORDS(NOFLOC+IPOINT) = LOCF(ADRESS) - LOCF(IWORDS) + 1
          IWORDS(NOFLEN+IPOINT) = LENGTH
          IWORDS(NOFTYP+IPOINT) = ITYPE
          CALL UCTOH  (KEYSTR, IWORDS(ITEMP), NCHWD, NCHKEY)
      ELSE
C
C        Not enough room for this key, sorry.
C
          WRITE (UNIT=LUNOUT, FMT=1003) KEYSTR(1:NCHKEY)
      END IF
      RETURN
 
1001  FORMAT
     * (' FFKEY  --- REPEATED DEFINITION OF ''', A, ''' - CALL IGNORED')
1002  FORMAT
     * (' FFKEY  --- INVALID LENGTH FOR ''', A, ''' - TRY AGAIN')
1003  FORMAT
     * (' FFKEY  --- NO SPACE TO DEFINE ''', A, ''' - CALL IGNORED')
 
      END
