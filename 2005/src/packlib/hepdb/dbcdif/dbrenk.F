*
* $Id: dbrenk.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbrenk.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBRENK (PATHN, KEYO, KEYN)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. DBRENK (PATHN, KEYO, KEYN)                              *
*                                                                      *
*   Changes the key elements of an existing object to new values       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KEYO     Array containing the old key elements                   *
*     KEYN     Array containing the new key elements                   *
*                                                                      *
*   Called by user,   DBFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =191 : Illegal path name                               *
*               =192 : Illegal KEYO values (no matching object)        *
*               =193 : No top directory found                          *
*               =194 : Error in getting the IO descriptor              *
*               =195 : Error in FZOUT in saving the journal file       *
*               =196 : Error in RZRENK in renaming key values          *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      DIMENSION       KEYO(9), KEYN(9), KEYSN(MXKYDX)
      CHARACTER       PATHN*(*)
*
*     ------------------------------------------------------------------
*
      CALL DBCKEY (KEYO, KEYSDX, MXKYDX)
      CALL DBCKEY (KEYN, KEYSN, MXKYDX)
      CALL CDRENK (PATHN, KEYSDX, KEYSN, IRC)
      IQUEST(1) = IRC
*                                                             END DBRENK
  999 END
