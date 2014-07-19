*
* $Id: ffrset.F,v 1.1.1.1 1996/03/08 11:50:42 mclareni Exp $
*
* $Log: ffrset.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:42  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FFRSET (LUNSUB, FFRSUB)
C
C      FUNCTIONAL DESCRIPTION:
C
C              This routines stores the address of FFRSUB (to be
C              declared EXTERNAL in the calling program). When a
C              data card READ is encountered, if the number of the
C              logical unit is -LUNSUB, then this routine will be
C              used to input the next data card.
C
C      DUMMY ARGUMENTS:
C
C              LUNSUB - absolute value of the negative logical unit
C                       associated with this subroutine. Must be:
C                             0 < LUNSUB < NFJUMP
C              FFRSUB - routine to be called when reading is directed
C                       to unit -LUNSUB via a READ data cards. This
C                       routine has the following calling sequence:
C
C                         SUBROUTINE FFRSUB(IBUSIZ)
C
C                         Where:
C
C                         IBUSIZ   - Length of the character string
C                                    to be returned with the data card,
C                                    blank padded.
C
C                         the string may be accessed using the common FFCHAR:
C
C                         COMMON / FFCHAR / STRING
C                         CHARACTER*132 STRING
C
C
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
C
C----------------- Beginning of executable statements -------------------------
C
C        Check if logical unit within range
C
      IF(LUNSUB.LE.0.OR.LUNSUB.GT.NFJUMP) THEN
        WRITE (UNIT=LUNOUT, FMT=1001) LUNSUB
        GO TO 999
      END IF
      KFJUMP(LUNSUB) = JUMPAD(FFRSUB)
 
 999  CONTINUE
 
1001  FORMAT
     * (' FFRSET --- INVALID VALUE', I12,
     *  ' FOR LUNSUB - ADDRESS NOT STORED')
      END
