*
* $Id: astdcc.F,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
*
* $Log: astdcc.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
       SUBROUTINE ASTDCC
*-----------------------------------------------------------------------
*   ASTDCC - Disables Control C AST
*   AUTHOR : Carlo Mekenkamp /Leiden (MEKENKAM@HLERUL5)
*   SIDE EFFECTS: Cancels control-C-AST on CHAN.
*   CREATION DATE: 10-MAR-1988
*-----------------------------------------------------------------------
#include "astcom.inc"
       INCLUDE '($SYSSRVNAM)/NOLIST'
       CALL SYS$CANCEL(%VAL(CHAN))
       END
