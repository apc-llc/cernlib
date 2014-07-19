*
* $Id: hnmset.F,v 1.1.1.1 1996/01/16 17:07:58 mclareni Exp $
*
* $Log: hnmset.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:58  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 13/05/92  11.23.56  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/12/91
      SUBROUTINE HNMSET(IDD, ITEM, IVAL)
*.==========>
*.
*.           Set in N-tuple IDD in the LNAME banks in all blocks
*.           the ITEM to IVAL.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      ID    = IDD
      IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF (IDPOS .LE. 0) THEN
         CALL HBUG('Unknown N-tuple','HNMSET',IDD)
         RETURN
      ENDIF
*
      LCID=LQ(LTAB-IDPOS)
*
*-- loop over all blocks
*
      LBLOK = LQ(LCID-1)
      LCHAR = LQ(LCID-2)
      LINT  = LQ(LCID-3)
      LREAL = LQ(LCID-4)
*
10    LNAME = LQ(LBLOK-1)
*
*-- loop over all variables in LNAME
*
      IOFF = 0
      NDIM = IQ(LBLOK+ZNDIM)
*
      DO 20 I = 1, NDIM
*
         IQ(LNAME+IOFF+ITEM) = IVAL
*
         IOFF = IOFF + ZNADDR
20    CONTINUE
*
      LBLOK = LQ(LBLOK)
      IF (LBLOK .NE. 0) GOTO 10
*
      END
