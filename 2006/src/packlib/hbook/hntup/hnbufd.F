*
* $Id: hnbufd.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnbufd.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 28/01/94  17.02.04  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/04/92
      SUBROUTINE HNBUFD(IDD)
*.==========>
*.
*.           Drop buffer manager buffers.
*.           For the data-structure description see routine HBNT.
*.
*.  This routine drops the contents buffers for N-tuple ID.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
*
      CALL HNTMPD(IDD)
*
      IF (LQ(LCDIR-4) .EQ. 0) RETURN
*
      IF (IDD .EQ. 0) THEN
         CALL MZDROP(IHDIV,LQ(LCDIR-4),'L')
         LQ(LCDIR-4) = 0
         LBUFM       = 0
         LBUF        = 0
      ELSE
*
*-- find buffer bank for ntuple IDD
*
         LBUF = LQ(LCDIR-4)
   20    IF (IQ(LBUF-5) .EQ. IDD) THEN
            CALL MZDROP(IHDIV,LBUF,' ')
            LBUF = LQ(LCDIR-4)
            GOTO 40
         ENDIF
         LBUF = LQ(LBUF)
         IF (LBUF .NE. 0) GOTO 20
*
*-- if buffers not found do not complain
*
**         CALL HBUG('Unknown N-tuple','HNBUFD',IDD)
         RETURN
      ENDIF
*
 40   END
 
