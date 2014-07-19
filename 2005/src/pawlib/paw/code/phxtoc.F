*
* $Id: phxtoc.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: phxtoc.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.00/00 12/10/92  10.54.31  by  Fons Rademakers
*-- Author : Fons Rademakers
      SUBROUTINE PHXTOC(IVAL,CVAL)
*.===========>
*.
*. Convert integer value IVAL into hexidecimal character string CVAL
*.
*..==========>
      CHARACTER*(*) CVAL
      PARAMETER (MAXDIG=16)
      CHARACTER*(MAXDIG) CHTEMP
*.______________________________________
*
      WRITE (CHTEMP,'(Z16)') IVAL
 10   IF(CHTEMP(1:1).EQ.' ')THEN
         I1=LENOCC(CHTEMP)
         CHTEMP=CHTEMP(2:I1)
         GOTO 10
      ENDIF
      I1=LENOCC(CHTEMP)
      CVAL='Z'''//CHTEMP(1:I1)//''''
*
      END
