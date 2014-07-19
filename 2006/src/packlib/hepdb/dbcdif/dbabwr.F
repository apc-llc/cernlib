*
* $Id: dbabwr.F,v 1.1.1.1 1996/02/28 16:24:54 mclareni Exp $
*
* $Log: dbabwr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:54  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBABWR (PATHN, LBK, LUN, NWKEY, KEY, KEYO, CHOPT)
*     ============================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBABWR (PATHN, *LBK*, LUN, NWKEY, KEY, KEYO, CHOPT)     *
*                                                                      *
*   Stores ASCII data from a disk file to the data base and also enters*
*   in the memory ala DBUSE in NODE/KEY structure                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK(*)   Address of Keys bank KYCD   (INPUT or OUTPUT)           *
*     LUN      Logical unit number of the file where ASCII data reside *
*     NWKEY    Number of keys associated with the data bank            *
*     KEY      Vector of keys                                          *
*     KEYO     Old vector of keys (for option X only)                  *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address if non-zero                           *
*          B   Save in the special backup file; not in standard Journal*
*          C   create Node/Key data structure ala DBUSE                *
*          X   Replaces an old set of keys                             *
*          7   Insertion time as supplied by user to be honoured       *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 61 : Too many keys                                   *
*               = 66 : Illegal logical unit number                     *
*               = 67 : File too long; no space in buffer               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#include "dxused.inc"
      DIMENSION       KEY(9), KEYO(9), LBK(9), KYXO(MXKYDX)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOP*28
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
*
* *** Reformat CHOPT
*
      IOPNDX = 0
      IOPTDX = 1
      IOPYDX = 1
      IF (IOPXDX.NE.0) THEN
        IOPRDX = 1
      ELSE
        IOPRDX = 0
      ENDIF
      IOPXDX = 0
      IOPPDX = 0
      IOPDDX = 0
      IOPSDX = 0
      IOPUDX = 0
      IF (IOKYDX(7).NE.0) THEN
        IOPHDX = 1
      ELSE
        IOPHDX = 0
      ENDIF
      CALL DBOPTM (CHOP)
      CALL DBCKEY (KEY, KEYSDX, MXKYDX)
      CALL DBCKEY (KEYO, KXYO, MXKYDX)
*
* *** Read in from the file
*
      LSTRCL(4) = 0
      CALL CDATOI (LUN, LSTRCL(4), IRC)
      IF (IRC.NE.0) THEN
        IF (LSTRCL(4).NE.0) CALL MZDROP (IDIVCD, LSTRCL(4), ' ')
        IQUEST(1) = IRC
        GO TO 999
      ENDIF
*
      IF (IOPRDX.NE.0) THEN
        CALL CDREPL (PATHN, LSTRCL(4), LBK, IDIVCD, KXYO, KEYSDX, CHOP,
     +               IRC)
      ELSE
        CALL CDSTOR (PATHN, LSTRCL(4), LBK, IDIVCD, KEYSDX, CHOP, IRC)
      ENDIF
      IF (LSTRCL(4).NE.0) CALL MZDROP (IDIVCD, LSTRCL(4), ' ')
      IQUEST(1) = IRC
*                                                             END DBABWR
  999 END
