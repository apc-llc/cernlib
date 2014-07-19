*
* $Id: fmquit.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmquit.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQUIT
#include "fatmen/fatsys.inc"
#include "fatmen/fstate.inc"
#include "fatmen/fatron.inc"
      PRINT 9001
9001  FORMAT(' *** FATMEN force termination. Use EXIT or END',
     +       ' to terminate normally')
      IF (THRONG(1:4) .NE. 'NONE') THEN
#if defined(CERNLIB_IBMVM)
      IF(SERMOD.NE.'A'.AND.SERMOD.NE.'?')
     +   CALL VMCMS('EXEC DROP '//SERMOD,IRC)
#endif
         CALL FMUPDT(0,0,1,IRC)
         MFPHAS = 2
         CALL FMEND(IRC)
      ENDIF
      END
