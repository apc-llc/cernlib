*
* $Id: fustar.F,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: fustar.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FUSTAR (KEY)
 
      DIMENSION KEY(1)
      CHARACTER*6 KEYSTR
C
C----------------- Beginning of executable statements -------------------------
C
      CALL FFGET ('NCHW', NCHW)
      CALL UHTOC(KEY, NCHW, KEYSTR, 6)
 
      WRITE (*, *)
     +    ' USER ACTION ROUTINE CALLED WITH KEY ''', KEYSTR, ''''
      WRITE (*, *)
 
      END
