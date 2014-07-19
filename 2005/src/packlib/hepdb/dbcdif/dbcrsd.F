*
* $Id: dbcrsd.F,v 1.1.1.1 1996/02/28 16:24:54 mclareni Exp $
*
* $Log: dbcrsd.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:54  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBCRSD (PATH, NKEX, CHFOR, CHTAG, MXKP, CHOPT)
*     =========================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBCRSD (PATH, NKEX, CHFOR, CHTAG, MXKP, CHOPT)          *
*                                                                      *
*   Creates a L3 directory with standard keys plus NKEX extra keys     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the pathname                *
*     NKEX     Number of user keys                                     *
*     CHFOR    Character string specifying the user key type           *
*     CHTAG    String of 8-character tags for the user key elements    *
*     MXKP     Maximum number of objects in a partitioned directory    *
*     CHOPT    Character string with any of the following characters   *
*          P   Create partitioned subdirectories for the pathname      *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 43 : Illegal number of user keys                     *
*                                                                      *
************************************************************************
*
#include "hepdb/cuserf.inc"
#include "dxused.inc"
      COMMON /QUEST/  IQUEST(100)
      CHARACTER*(*)   PATH, CHTAG(*), CHFOR, CHOPT
      CHARACTER       CHOP*28
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPCDX = 1
      CALL DBOPTM (CHOP)
*
      CALL CDMDIR (PATH,NKEX, CHFOR,CHTAG,MXKP, IPRECU,DELTCU, CHOP,IRC)
      IQUEST(1) = IRC
*                                                             END DBCRSD
  999 END
