*
* $Id: astxit.F,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
*
* $Log: astxit.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
       SUBROUTINE ASTXIT
*-----------------------------------------------------------------------
*   ASTXIT - Exitialisation Control C AST routines
*   AUTHOR : Carlo Mekenkamp /Leiden (MEKENKAM@HLERUL5)
*   SIDE EFFECTS: Disable control-C AST,
*                  Deassign channel to terminal.
*   CREATION DATE: 10-MAR-1988
*-----------------------------------------------------------------------
#include "astcom.inc"
       INCLUDE '($SYSSRVNAM)/NOLIST'
       CALL ASTDCC
       CALL SYS$DASSGN(%VAL(CHAN))
       END
