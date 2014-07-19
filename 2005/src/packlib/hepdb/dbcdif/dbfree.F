*
* $Id: dbfree.F,v 1.1.1.1 1996/02/28 16:24:57 mclareni Exp $
*
* $Log: dbfree.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:57  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBFREE (PATHN, LBK, KEYS, CHOPT)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. DBFREE (PATHN, LBK, KEYS, CHOPT)                        *
*                                                                      *
*   Declares the given data bank(s) as candidates to be dropped in     *
*   case space is needed in the database division. Optionally it       *
*   deletes the Data bank(s) (with option D) or the Keys as well as    *
*   the Data bank(s) (with option K).                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK(*)   Address(es) of Keys bank(s) KYCD                        *
*     KEYS     Vector of keys                                          *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address(es) if non-zero                       *
*          D   drop the Data bank(s) supported at link 1 of Key bank(s)*
*          K   drop the Key bank(s) as well as Data bank(s)            *
*          M   expect multiple Key banks                               *
*          S   Key/data was retrieved using 'S' option in DBUSE        *
*          3   start valdity time to be < KEYS(3) (with option S)      *
*          4   end   valdity time to be > KEYS(4) (with option S)      *
*          5   specific Program version number required                *
*          7   insertion time to be < KEYS(7)                          *
*          n   consider user key n (7 < n < 29)                        *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 51 : Illegal character option                        *
*               = 52 : No access to the Key banks                      *
*               = 53 : Pathname not found in the RZ directory          *
*               = 54 : Pathname not matched to that found in bank NOCD *
*               = 55 : Too many keys with option M                     *
*               = 56 : Illegal Key option                              *
*               = 57 : Illegal pathname                                *
*               = 58 : Database structure in memory clobbered          *
*               = 59 : Some of the expected key banks not found        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      CHARACTER       PATHN*(*), CHOPT*(*), CHOP*28
      DIMENSION       LBK(9), KEYS(9)
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
      CALL DBOPTM (CHOP)
*
      CALL DBCKEY (KEYS, KEYSDX, MXKYDX)
*
      CALL CDFREE (PATHN, LBK, MASKDX, KEYSDX, CHOP, IRC)
      IQUEST(1) = IRC
*                                                             END DBFREE
  999 END
