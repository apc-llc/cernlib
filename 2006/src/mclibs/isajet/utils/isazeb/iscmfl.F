*
* $Id: iscmfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: iscmfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISCMFL(CUNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FILL THE
C-
C-   Inputs  :
C-   CUNIT = unit number for command file
C-
C-   Created  13-DEC-1989   Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER MAXLIN
      PARAMETER( MAXLIN =  200 )
      CHARACTER LINE(MAXLIN)*80
      INTEGER NCHAR(MAXLIN)
      INTEGER LISCM,CUNIT,IER
      INTEGER NLINE,NDATA,I1,I2,L1,IPT,ILINE
#include "zebcom.inc"
C----------------------------------------------------------------------
C
C ****  READ IN COMMAND FILE
C
      REWIND(CUNIT)
      NLINE = 0
      NDATA = 0
    1 NLINE = NLINE + 1
      READ (CUNIT,'(A80)',END=2)LINE(NLINE)
      CALL SWORDS(LINE(NLINE),I1,I2,L1)
      LINE(NLINE) = LINE(NLINE)(1:I2)//CHAR(13)
      NCHAR(NLINE) = I2 + 1
      NDATA = NDATA + ( NCHAR(NLINE) + 3 ) / 4
      IF( NLINE.LT.MAXLIN) GOTO 1
    2 CALL BKISCM(NDATA+1,LISCM)
      REWIND(CUNIT)
      IPT = LISCM + 1
C
C ****  FILL ISCM WITH COMMAND FILE
C
      DO ILINE = 1, NLINE
        CALL UCTOH(LINE(ILINE)(1:NCHAR(ILINE)),IQ(IPT),4,NCHAR(ILINE))
        IPT = IPT + ( NCHAR(ILINE) + 3 ) / 4
      END DO
  999 RETURN
      END
