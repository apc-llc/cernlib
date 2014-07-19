*
* $Id: newguy.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: newguy.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE NEWGUY (TEXT,ROUT)

#include "kerngent/mkcde.inc"

      CHARACTER    TEXT*(*), ROUT*(*)


      CALL PRTEST
      NFAIL = 0

      WRITE (ITB,9000)
 9000 FORMAT (1X/1X,69(1H-))

      WRITE (ITB,9020) ROUT, TEXT
 9020 FORMAT (/' Routine ',A, 'testing  ',A)

      WRITE (ITB,9021)
 9021 FORMAT (1X)
      RETURN
      END
