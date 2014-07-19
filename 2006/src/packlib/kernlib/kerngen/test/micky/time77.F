*
* $Id: time77.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: time77.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TIME77 (NCALLS,CHNAME,NITEMS,CHITEM)

#include "kerngent/mkcde.inc"

      CHARACTER    CHNAME*(*), CHITEM*(*)
      CHARACTER    SRNAME*6,   ITEM*8


      CALL TIMED (TIMERD)
      MILLIS = 1000. * TIMERD

      SRNAME = CHNAME
      ITEM   = CHITEM

      IF (NTEST.NE.0)        CALL PRTEST

      IF (NITEMS.GT.0)  THEN
          WRITE (ITB,9001) NCALLS,SRNAME,NITEMS,ITEM,MILLIS
        ELSE
          WRITE (ITB,9002) NCALLS,SRNAME,MILLIS
        ENDIF
      RETURN

 9001 FORMAT (1X,I12,' calls to ',A,' for',I6,1X,A,'  took',I6,
     F' milli-seconds.')
 9002 FORMAT (1X,I12,' calls to ',A,'  took',I6,' milli-seconds.')
      END
