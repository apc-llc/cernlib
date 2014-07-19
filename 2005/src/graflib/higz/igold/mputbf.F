*
* $Id: mputbf.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: mputbf.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE MPUTBF(CHARST,LCH)
#include "higz/golden.inc"
      CHARACTER*(*)  CHARST
*
      IF (LCH .LE. 0 .OR. LCH .GT. LENOCC(CHARST)) RETURN
      DO 10  I=1,LCH
         IF (IPOINT .EQ. LBUF+1) THEN
            CALL MOUTBF
            IPOINT = 1
         END IF
         CHMBUF(IPOINT:IPOINT) = CHARST(I:I)
         IPOINT = IPOINT + 1
   10 CONTINUE
      END
#endif
