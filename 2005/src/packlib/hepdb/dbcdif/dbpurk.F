*
* $Id: dbpurk.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbpurk.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBPURK (PATHN, ITIME, KEYS, CHOPT)
*     =============================================
*
************************************************************************
*                                                                      *
*        SUBR. DBPURK (PATHN, ITIME, KEYS, CHOPT)                      *
*                                                                      *
*   Deletes objects in a directory path name steered by a selection    *
*   on a number of key elements                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     ITIME    Time of validity of the object                          *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          S   expect multiple Key banks satisfying selection on a     *
*              number of keys                                          *
*          3   selects objects with start validity time < KEYS(3)      *
*          4   selects objects with end validity time > KEYS(4)        *
*          5   specific Program version number required                *
*          7   select objects with insertion time < KEYS(7)            *
*          n   consider user key n (where 7 < n < 29 )                 *
*                                                                      *
*   Called by user,   DBFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =111 : Illegal path name                               *
*               =112 : No key for the path name satisfying the Key     *
*                      assignments                                     *
*               =113 : Illegal character option                        *
*               =114 : Valid data objects in the Node/Key structure    *
*                                                                      *
*     If IQUEST(1) =0, IQUEST(2) carries information on number of      *
*     data objects deleted in the disk                                 *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      DIMENSION       ITIME(9), KEYS(9)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOPF*28
*
*     ------------------------------------------------------------------
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      CALL DBOPTM (CHOPF)
      CALL DBCKEY (KEY, KEYSDX, MXKYDX)
*
      CALL CDPURK (PATHN, ITIME, MASKDX, KEYSDX, CHOPF, IRC)
      IQUEST(1) = IRC
*                                                             END DBPURK
  999 END
