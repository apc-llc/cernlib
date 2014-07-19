*
* $Id: astcch.F,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
*
* $Log: astcch.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
       INTEGER*4 FUNCTION ASTCCH(SA, MA)
*-----------------------------------------------------------------------
*   ASTCCH - This routine gets control if an exception occurs
*             when established
*   AUTHOR : Carlo Mekenkamp /Leiden (MEKENKAM@HLERUL5)
*   VARIABLES : SA          : Signal Array
*                              SA(1)      Number of arguments
*                              SA(2)      Condition name
*                              SA(3)      First signal-specific argument
*                              ...
*                              SA(SA(1))     PC at time exception
*                              SA(SA(1)+1)   PSL at time exception
*                MA          : Mechanism Array
*                              MA(1) Number of mechanism arguments
*                              MA(2) Establisher frame address
*                              MA(3) Frame depth of establisher
*                              MA(4) Saved register R0
*                              MA(5) Saved register R1
*   SIDE EFFECTS: IF condition matches SS$_CONTROLC THEN
*                     IF NOT ASTCS THEN
*                        stack unwind to establisher of caller
*                        enable control-C AST
*                     ELSE
*                        ASTIP=.TRUE.
*                        return SS$_CONTINUE
*                     ENDIF
*                  ELSE
*                     resignal
*                  ENDIF
*   CREATION DATE: 10-MAR-1988
*-----------------------------------------------------------------------
#include "astcom.inc"
       INTEGER*4 SA(*), MA(5)
       INCLUDE '($SYSSRVNAM)/NOLIST'
       INCLUDE '($LIBDEF)/NOLIST'
       INCLUDE '($SSDEF)/NOLIST'
       INTEGER*4 LIB$MATCH_COND
       IF(LIB$MATCH_COND(SA(2),SS$_CONTROLC).EQ.1)THEN
            IF(ASTCS)THEN
                 ASTIP = .TRUE.
                 ASTCCH = SS$_CONTINUE
            ELSE
                 CALL SYS$UNWIND(MA(3),)
                 CALL ASTECC
            ENDIF
       ELSE
            ASTCCH = SS$_RESIGNAL
       ENDIF
       END
