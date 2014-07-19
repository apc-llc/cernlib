*
* $Id: chkver.F,v 1.1.1.1 1996/02/15 17:54:54 mclareni Exp $
*
* $Log: chkver.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE CHKVER

C-    Check of the checking routines

#include "kerngent/mkcde.inc"

      DIMENSION CHECK(10)

      DATA  CHECK/0.,1.,2.,3.,4.,5.,6.,7.,8.,9./


      WRITE (ITB,9071)
      CALL MVERII (1,INTG,INTG,10)
      CALL MVERII (2,INTG(2),INTG,10)
      CALL MVERIF (1,CHECK,CHECK,10)
      CALL MVERIF (2,CHECK(2),CHECK,9)
      CALL PRTEST
      NFAILT = 0
      NFAIL  = 0
      RETURN

 9071 FORMAT (/' First of all, check the checking routine:'/
     F' The first test should be ok, the second should fail.'/)
      END
