*
* $Id: getenvf.F,v 1.1.1.1 1996/02/15 17:47:44 mclareni Exp $
*
* $Log: getenvf.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:44  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "_kernbit/pilot.h"
#if defined(CERNLIB_IBMMVS)
      SUBROUTINE GETENVF(CHENV,CHVAL)
*     Dummy version for systems without environmental variables
      COMMON/SLATE/IS(40)
      CHARACTER*(*)  CHENV,CHVAL
      CHVAL = ' '
      IS(1) = 0
      END
#endif
