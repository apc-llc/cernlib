*
* $Id: hndesc.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hndesc.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/08 07/09/93  19.44.33  by  Fons Rademakers
*-- Author :    Fons Rademakers   07/09/93
      SUBROUTINE HNDESC(IOFF, NSUB, ITYPE, ISIZE, NBITS, INDVAR)
*.==========>
*.
*.       Return information stored in the Ntuple descriptor word.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcnt.inc"
#include "hbook/hcbook.inc"
*
      LOGICAL INDVAR
*
#include "hbook/jbyt.inc"
*
      NSUB  = JBYT(IQ(LNAME+IOFF+ZDESC), 18, 3)
      ITYPE = JBYT(IQ(LNAME+IOFF+ZDESC), 14, 4)
      ISIZE = JBYT(IQ(LNAME+IOFF+ZDESC), 8,  6)
      NBITS = JBYT(IQ(LNAME+IOFF+ZDESC), 1,  7)
*
      INDVAR = .FALSE.
      IF (JBIT(IQ(LNAME+IOFF+ZDESC),28) .EQ. 1) INDVAR = .TRUE.
*
*-- fix the NBITS for character variables (7 bits was not enough)
*
      IF (ITYPE .EQ. 5) NBITS = IBIPB*ISIZE
*
      END
