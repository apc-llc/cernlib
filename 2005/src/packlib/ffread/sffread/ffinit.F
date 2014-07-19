*
* $Id: ffinit.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffinit.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFINIT (NW)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routine intializes the FFREAD package.
C              If any keys had been previously specified,
C              they are erased with all associated information.
C
C      DUMMY ARGUMENTS:
C
C              NW specifies the number of words allocated by the
C              user in common CFREAD. This can be used to increase
C              the number of available keys.
C
C      IMPLICIT INPUTS:
C
C              NONE
C
C      IMPLICIT OUTPUTS:
C
C              The common CFREAD is initialized with default information.
C
C      SIDE EFFECTS:
C
C              Any information set up by previous calls to FFREAD routines
C              is destroyed. Users of FFREAD (which calls FFINIT with
C              NW=0) can call FFINIT(NW) before calling FFREAD.
C
      EXTERNAL FFUSER
C
#include "ffread/ffmach.inc"
#include "ffread/ffluns.inc"
#include "ffread/cfread.inc"
#include "ffread/ffcomm.inc"
C
C--- Define initial value for NWORDS (so FFREAD users can call FFINIT)
      DATA NWINIT/ NCSIZE/
C----------------- Beginning of executable statements -------------------------
C
C        Initialize default unit numbers and input file stack
C
      LINPUT = LENDEF
      LUNIN  = LUNDIN
      LUNOUT = LUNDUT
      ISTACK = 0
C
C        Initialize jump addresses
C
      CALL VFILL(KFJUMP,NFJUMP,IQANIL)
      CALL FFUSET(FFUSER)
C
C        Default key size and associated values
C
      NCHKEY = NCHKYD
      NKEYS  = 0
      NWDKEY = (NCHKEY+NCHWD-1) / NCHWD
C
C        Set size of common, check for at least one key possible if NW not 0
C
      IF (NW .EQ. 0) THEN
          NWORDS = NWINIT
      ELSE
          IF (NW .GE. NOVERH+NWDKEY+NKYOVH) THEN
              NWORDS = NW - NOVERH
          ELSE
              WRITE (UNIT=LUNOUT, FMT=1001) NW
              NWORDS = NCSIZE
          END IF
C        Remember NWORDS in NWINIT for later FFINIT(0) call by FFREAD
          NWINIT= NWORDS
      END IF
 
      MAXKEY = NWORDS / (NWDKEY + NKYOVH)
C
C        Calculate offsets into IWORDS for different arrays
C
      NOFLEN = MAXKEY   -      1 + 1
      NOFTYP = MAXKEY*2 -      1 + 1
      NOFKEY = MAXKEY*3 - NWDKEY + 1
C
C        Initialize character options for FFGET, FFKEY, and FFSET.
C
      CALL UCTOH ('SIZE', IFFOPT(1), NCHWD, 4)
      CALL UCTOH ('LENG', IFFOPT(2), NCHWD, 4)
      CALL UCTOH ('LINP', IFFOPT(3), NCHWD, 4)
      CALL UCTOH ('LOUT', IFFOPT(4), NCHWD, 4)
      CALL UCTOH ('NBIT', IFFOPT(5), NCHWD, 4)
      CALL UCTOH ('NCHW', IFFOPT(6), NCHWD, 4)
      CALL UCTOH ('NBCH', IFFOPT(7), NCHWD, 4)
      CALL UCTOH ('NCHK', IFFOPT(8), NCHWD, 4)
 
      CALL UCTOH ('INTE', KEYOPT(1), NCHWD, 4)
      CALL UCTOH ('REAL', KEYOPT(2), NCHWD, 4)
      CALL UCTOH ('LOGI', KEYOPT(3), NCHWD, 4)
C
C        Reset error counter
C
      FFFLAG = 0.
      RETURN
 
1001  FORMAT
     * (' FFINIT --- INVALID PARAMETER NW =', I12,
     *  ' - DEFAULT ASSUMED')
 
      END
