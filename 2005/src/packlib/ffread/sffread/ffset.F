*
* $Id: ffset.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffset.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFSET (CHOPT, IVALUE)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This sets some values which are optional within FFREAD.
C
C      DUMMY ARGUMENTS:
C
C              CHOPT  - Character argument specifying which item to change.
C                       Valid are:
C                         LENG - set # of significant characters on data cards
C                         LINP - change input  LUN
C                         LOUT - change output LUN
C                         SIZE - set key size in number of characters.
C                                This must be at least 4 and cannot be
C                                greater than NCHMAX.
C                       The last option, SIZE, can only be specified
C                       after a call to FFINIT and before the first
C                       call to FFKEY/FFGO.
C              IVALUE - Value of the desired argument
C
C      IMPLICIT INPUTS:
C
C              NONE
C
C      IMPLICIT OUTPUTS:
C
C              The appropriate value in common CFREAD is changed.
C
C      SIDE EFFECTS:
C
C              NONE
C
#include "ffread/ffmach.inc"
#include "ffread/ffluns.inc"
#include "ffread/ffcomm.inc"
#include "ffread/cfread.inc"
      CHARACTER CHOPT*(*)
C
C----------------- Beginning of executable statements -------------------------
C
C        Check option and dispatch on it
C
      CALL UCTOH (CHOPT, NCHOPT, NCHWD, MIN (LEN (CHOPT), 4) )
      IOPT = IUCOMP (NCHOPT, IFFOPT, 4)
      GO TO (1, 2, 3, 4), IOPT
 
      WRITE (UNIT=LUNOUT, FMT=1005) NCHOPT
      RETURN
 
   1  IF (NKEYS .EQ. 0) THEN
          IF (     IVALUE .GE.       4
     *       .AND. IVALUE .LE.  NCHMAX
     *       .AND. IVALUE .LT. (NWORDS-NKYOVH)*NCHWD) THEN
              NCHKEY = IVALUE
              NWDKEY = (NCHKEY+NCHWD-1) / NCHWD
              MAXKEY = NWORDS / (NWDKEY + NKYOVH)
              NOFLEN = MAXKEY   -      1 + 1
              NOFTYP = MAXKEY*2 -      1 + 1
              NOFKEY = MAXKEY*3 - NWDKEY + 1
          ELSE
              WRITE (UNIT=LUNOUT, FMT=1001) IVALUE
          END IF
      ELSE
          WRITE (UNIT=LUNOUT, FMT=1002)
      END IF
      RETURN
 
   2  IF (IVALUE .GE. LENMIN .AND. IVALUE .LE. LENMAX) THEN
          LINPUT = IVALUE
      ELSE
          WRITE (UNIT=LUNOUT, FMT=1003) IVALUE
      END IF
      RETURN
 
   3  IF (IVALUE .GE. -NFJUMP .AND. IVALUE .LE. LUNMAX) THEN
          LUNIN  = IVALUE
      ELSE
          WRITE (UNIT=LUNOUT, FMT=1004) IVALUE, 'INPUT'
      END IF
      RETURN
 
   4  IF (IVALUE .GE. 0 .AND. IVALUE .LE. LUNMAX) THEN
          LUNOUT = IVALUE
      ELSE
          WRITE (UNIT=LUNOUT, FMT=1004) IVALUE, 'OUTPUT'
      END IF
      RETURN
 
1001  FORMAT
     * (' FFSET  --- INVALID VALUE', I12,
     *  ' FOR KEY SIZE - NOT CHANGED')
1002  FORMAT
     * (' FFSET  --- INVALID STATE FOR OPTION ''SIZE'' - IGNORED')
1003  FORMAT
     * (' FFSET  --- INVALID VALUE', I12,
     *  ' FOR CARD LENGTH - NOT CHANGED')
1004  FORMAT
     * (' FFSET  --- INVALID LUN', I12,
     *  ' FOR ', A, ' - NOT CHANGED')
1005  FORMAT
     * (' FFSET  --- UNRECOGNIZED OPTION ''', A4, ''' - TRY AGAIN')
 
      END
