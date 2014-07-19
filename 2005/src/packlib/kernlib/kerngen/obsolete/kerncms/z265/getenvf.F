*
* $Id: getenvf.F,v 1.1.1.1 1996/02/15 17:51:53 mclareni Exp $
*
* $Log: getenvf.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:53  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GETENVF(CHENV,CHVAL)
C
C CERN PROGLIB#    Z265 GETENVF         .VERSION KERNCMS  1.06  910716
C ORIG. 11/07/91, Jamie Shiers
C
      COMMON/SLATE/IS(40)
      CHARACTER*(*)  CHENV,CHVAL
      INTEGER SYSTEMF
      IRC=SYSTEMF('EXEC SETENV '//CHENV(1:LEN(CHENV))//' (STACK')
      CALL VMRTRM(CHVAL,IS(1))
      END
