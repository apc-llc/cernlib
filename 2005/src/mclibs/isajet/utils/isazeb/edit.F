*
* $Id: edit.F,v 1.10 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: edit.F,v $
* Revision 1.10  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      LOGICAL FUNCTION EDIT(I)
C          DUMMY ROUTINE FOR USER DEFINED LOGICAL FUNCTION
C          EDIT=.TRUE. TO WRITE EVENT ON TAPE
C          EDIT=.FALSE. TO SUPPRESS WRITING
      EDIT=.TRUE.
      RETURN
      END
