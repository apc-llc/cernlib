*
* $Id: hntmpd.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hntmpd.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 28/01/94  17.02.41  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNTMPD(IDD)
*.==========>
*.
*.           Drop temporary buffers.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine drops the temporary buffers for N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
*
      IF (LQ(LCDIR-5) .EQ. 0) RETURN
*
      IF (IDD .EQ. 0) THEN
         CALL MZDROP(IHDIV,LQ(LCDIR-5),'L')
         LQ(LCDIR-5) = 0
         LTMPM       = 0
         LTMP        = 0
      ELSE
*
*-- find tmp buffer bank for ntuple IDD
*
         LTMP = LQ(LCDIR-5)
   20    IF (IQ(LTMP-5) .EQ. IDD) THEN
            CALL MZDROP(IHDIV,LTMP,' ')
            LTMP = LQ(LCDIR-5)
            GOTO 40
         ENDIF
         LTMP = LQ(LTMP)
         IF (LTMP .NE. 0) GOTO 20
*
*-- if buffers not found don't complain
*
**         CALL HBUG('Unknown N-tuple','HNTMPD',IDD)
         RETURN
      ENDIF
*
 40   END
 
