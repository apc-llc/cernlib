*
* $Id: dbget.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbget.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBGET (PATHN, LBK, KEYS, CHOPT)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. DBGET (PATHN, LBK*, KEYS, CHOPT)                        *
*                                                                      *
*   Prepares the database data structure in memory for any required    *
*   Pathname and set of Keys, unless already done.  Returns the        *
*   in memory for the corresponding Key bank(s) with a selection on    *
*   a range of start validity time and user keys.                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK(*)   Address(es) of Keys bank(s) KYCD. The data bank address *
*              can be obtained as LQ(LBK-1)                            *
*              For option 'S' it is the support address of the linear  *
*              structure                                               *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     CHOPT    Character string with any of the following characters   *
*          K   read only the keys (no data is required)                *
*          S   expect multiple Key banks satisfying selection on a     *
*              number of keys                                          *
*          3   selects objects with start validity time > KEYS(3)      *
*          4   selects objects with start validity time < KEYS(4)      *
*          5   specific Program version number required                *
*          6   select objects with insertion time > KEYS(6)            *
*          7   select objects with insertion time < KEYS(7)            *
*          n   consider user key n (where 7 < n < 29 )                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               =  2 : Illegal path name                               *
*               = 32 : No keys/data in this directory                  *
*                                                                      *
*     If IQUEST(1) =0, IQUEST(2) carries information on number of      *
*     data objects selected by DBGET                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      DIMENSION       KEYS(9), LBK(9), KYSR(2)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOP*28
*
*     ------------------------------------------------------------------
*
* *** Reformat character options
*
      CALL DBOPTS (CHOPT, IRC)
      IF (IRC.NE.0) THEN
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
      IOPRDX = 0
      CALL DBOPTM (CHOP)
*
      CALL DBCKEY (KEYS, KEYSDX, MXKYDX)
*
      KYINM   = KEYS(6)
      KYSR(1) = 1
      KYSR(2) = IBIGCD
      CALL CDGETDB (PATHN, LBK, MASKDX, KEYSDX, KYSR, KYINM, CHOPT, IRC)
      IQUEST(1) = IRC
*                                                              END DBGET
  999 END
