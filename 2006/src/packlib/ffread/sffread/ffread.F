*
* $Id: ffread.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffread.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFREAD (NKEY, KEY, LOCVAR, LENVAR)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This is the old-fashioned form of using FFREAD and is
C              only supplied for backward compatibility.
C
C              If NKEY is set to 0, it is used to modify the values for
C              the default input/output LUNs.
C
C              If NKEY is negative, it will return them.
C
C              If NKEY is positive, it specifies the number of keys
C              in KEY. The keys will be set up and FFGO called to do
C              the work.
C
C      DUMMY ARGUMENTS:
C
C              NKEY   - number of keys. Different actions depending on
C                       sign of NKEYS are described above.
C              KEY    - array containing user's keys, one per machine word,
C                       with NCHKYD (four) significant characters.
C              LOCVAR - locations of variables/arrays to change as returned
C                       by LOCF.
C              LENVAR - length of array at location given in LOCVAR.
C
C      IMPLICIT INPUTS:
C
C              The data cards entered by the user.
C
C      IMPLICIT OUTPUTS:
C
C              The data cards read modify appropriate locations in memory.
C
C      SIDE EFFECTS:
C
C              NONE
C
#include "ffread/ffmach.inc"
#include "ffread/ffluns.inc"
#include "ffread/cfread.inc"
      INTEGER KEY(1), LOCVAR(1), LENVAR(1)
 
      CHARACTER KEYSTR*(NCHKYD)
 
      INTEGER LFFIN, LFFOUT
      DATA LFFIN / LUNDIN /, LFFOUT / LUNDUT /
C
C----------------- Beginning of executable statements -------------------------
C
C        NKEY < 0: Return values of input/output LUNs
C
      IF (NKEY .LT. 0) THEN
          LOCVAR(1) = LFFIN
          LENVAR(1) = LFFOUT
          RETURN
      END IF
C
C        NKEY = 0: Set values of input/output LUNs
C
      IF (NKEY .EQ. 0) THEN
          IF (LOCVAR(1) .NE. 0) LFFIN  = LOCVAR(1)
          IF (LENVAR(1) .NE. 0) LFFOUT = LENVAR(1)
          RETURN
      END IF
C
C        Here for the real work. First, initialize.
C
      CALL FFINIT (0)
      IF (LFFIN  .NE. LUNDIN) CALL FFSET ('LINP', LFFIN)
      IF (LFFOUT .NE. LUNDUT) CALL FFSET ('LOUT', LFFOUT)
C
C        Extract the keys and call FFKEY for each.
C
      DO 1 I = 1, NKEY
          CALL UHTOC (KEY(I), NCHKYD, KEYSTR, NCHKYD)
          IADRES = LOCVAR(I) - LOCF(IWORDS) + 1
          CALL FFKEY (KEYSTR, IWORDS(IADRES), LENVAR(I), 'NONE')
    1 CONTINUE
C
C        Now let FFGO do the rest.
C
      CALL FFGO
      RETURN
 
      END
