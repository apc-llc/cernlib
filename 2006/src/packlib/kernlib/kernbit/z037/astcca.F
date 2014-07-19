*
* $Id: astcca.F,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
*
* $Log: astcca.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
       SUBROUTINE ASTCCA
*-----------------------------------------------------------------------
*   ASTCCA - This routines receives control when a control_c is typed
*              to the terminal.
*   AUTHOR : Carlo Mekenkamp /Leiden (MEKENKAM@HLERUL5)
*   SIDE EFFECTS: Signal SS$_CONTROLC.
*   CREATION DATE: 10-MAR-1988
*-----------------------------------------------------------------------
#include "astcom.inc"
       INCLUDE '($SYSSRVNAM)/NOLIST'
       INCLUDE '($SSDEF)/NOLIST'
       INCLUDE '($LIBDEF)/NOLIST'
       INCLUDE '($STSDEF)/NOLIST'
       CALL LIB$SIGNAL(%VAL(IOR(IAND(-(STS$M_SEVERITY+1),
     -      SS$_CONTROLC),STS$K_ERROR)))
       END
