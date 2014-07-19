*
* $Id: kbhdr.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: kbhdr.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE KBHDR(STRING)
      CHARACTER*(*) STRING
      PRINT 1000,STRING
1000  FORMAT(' **** Testing ',A)
      RETURN
      END
