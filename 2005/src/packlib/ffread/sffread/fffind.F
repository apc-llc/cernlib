*
* $Id: fffind.F,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: fffind.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      INTEGER FUNCTION FFFIND (NEWSTR)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This function searches all keys in common CFREAD
C              for a match with the key in NEWSTR.
C
C      DUMMY ARGUMENTS:
C
C              NEWSTR contains the character string to compare the keys with.
C
C      IMPLICIT INPUTS:
C
C              The key list in common CFREAD. Comparison is made up
C              to the length specified by NCHKEY.
C
C      IMPLICIT OUTPUTS:
C
C              NONE
C
C       FUNCTION VALUE:
C
C              If the key in NEWSTR is found in the list, the index
C              is returned. If the key is not found, the negative index
C              of the key value which would follow this key is returned.
C              This is used for insertion of keys.
C
C      SIDE EFFECTS:
C
C              NONE
C
#include "ffread/ffmach.inc"
#include "ffread/cfread.inc"
      CHARACTER NEWSTR*(*)
      CHARACTER COMPAR*(NCHMAX), OLDSTR*(NCHMAX)
C
C        Loop over all keys to find given one.
C        In case of no keys, the loop isn't executed and
C        the statement after the loop is correct.
C
      COMPAR(1:NCHKEY) = NEWSTR
      DO 1 I = 1, NKEYS
          CALL UHTOC (IWORDS(NOFKEY+I*NWDKEY), NCHWD, OLDSTR, NCHKEY)
          IF (LGT (COMPAR(1:NCHKEY), OLDSTR(1:NCHKEY)) ) GO TO 1
          IF (LLT (COMPAR(1:NCHKEY), OLDSTR(1:NCHKEY)) ) GO TO 2
C
C        Found a match - return its index
C
          FFFIND = I
          RETURN
C
C        Step to next key
C
   1  CONTINUE
C
C        No match, this key comes after the last known key
C
      FFFIND = - (NKEYS+1)
      RETURN
C
C        No match, return index of next higher key for insertion
C
   2  FFFIND = - I
      RETURN
 
      END
