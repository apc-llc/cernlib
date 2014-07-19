*
* $Id: hnbptr.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnbptr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 27/05/92  15.58.16  by  Fons Rademakers
*-- Author :    Fons Rademakers   27/02/92
      INTEGER FUNCTION HNBPTR(BLKNA1)
*.==========>
*.
*.           Return the LBLOK of the block with name BLKNAM
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) BLKNA1
      CHARACTER*8   BLKNAM
      INTEGER       IBLKN(2)
*
      BLKNAM = BLKNA1
      CALL CLTOU(BLKNAM)
      HNBPTR = 0
*
      CALL UCTOH(BLKNAM, IBLKN, 4, 8)
*
      LL = LQ(LCID-1)
10    IF (IBLKN(1).EQ.IQ(LL+ZIBLOK) .AND.
     +    IBLKN(2).EQ.IQ(LL+ZIBLOK+1)) THEN
         HNBPTR = LL
         RETURN
      ENDIF
      LL = LQ(LL)
      IF (LL .NE. 0) GOTO 10
*
      END
