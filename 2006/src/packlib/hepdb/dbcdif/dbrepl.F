*
* $Id: dbrepl.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbrepl.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBREPL (PATHN, LBK, LBD, IUDIV, LSUP, NWKEY, KEYO, KEYN
     +                 , IPREC, CHOPT)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBREPL (PATHN, *LBK*, LBD*, IUDIV, LSUP, NWKEY, KEYO,   *
*                      KEYN, IPREC, CHOPT)                             *
*                                                                      *
*   Stores data from memory to disk and also enters in the memory ala  *
*   DBUSE in NODE/KEY structure. Also replaces an Old set of keys in   *
*   KEYO by a new set of keys in KEYN                                  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK(*)   Address of Keys bank KYCD   (INPUT or OUTPUT)           *
*     LBD(*)   Address of Data bank                                    *
*     IUDIV    Division index of the user data bank                    *
*     LSUP     Address of bank in memory where data reside             *
*     NWKEY    Number of keys associated with the data bank            *
*     KEYO     Vector of old keys                                      *
*     KEYN     Vector of new keys                                      *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*     CHOPT    Character string with any of the following characters   *
*          A   trust LBK address if non-zero                           *
*          B   Save in the special backup file; not in standard Journal*
*          C   create Node/Key data structure ala DBUSE                *
*          F   Updates with a fully matched data object (in user keys) *
*          K   Store data only inside the keys (not yet installed)     *
*          N   Create new (sub)directory(ies)                          *
*          P   Create partitioned subdirectories for the pathname      *
*          R   Store with full RZ option (No compression to be made)   *
*          S   Create stand alone (master) data                        *
*          T   Special text type of data (to be used with R)           *
*          U   Store data uncompressed                                 *
*          Z   Store only nonzero elements. An element is considered to*
*              be zero if its absolute value is less than IPREC (real) *
*          7   Insertion time as supplied by user to be honoured       *
*                                                                      *
*   Called by user,   DBAIWR, DBUPKY                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 61 : Too many keys                                   *
*               = 62 : Too many keys with option N                     *
*               = 63 : Data base structure in memory clobbered         *
*               = 64 : Error in MZCOPY while copying Data bank         *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      DIMENSION       KEYN(9), KEYO(9), LBD(9), LBK(9), LSUP(9)
      DIMENSION       KEYSN(MXKYDX)
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
      IF (IOPRDX.NE.0) THEN
        IOPYDX = 1
        IOPRDX = 0
      ENDIF
      IF (IOPYDX.NE.0.OR.IOPTDX.NE.0) THEN
        IOPPDX = 0
      ELSE
        IOPPDX = 1
      ENDIF
      IF (IOPSDX.NE.0) THEN
        IOPDDX = 0
        IOPSDX = 0
      ELSE
        IOPDDX = 1
      ENDIF
      IF (IOPUDX.NE.0) THEN
        IOPPDX = 0
        IOPUDX = 0
      ENDIF
      IF (IOKYDX(7).NE.0) THEN
        IOPHDX = 1
      ELSE
        IOPHDX = 0
      ENDIF
      CALL DBOPTM (CHOP)
*
      CALL DBCKEY (KEYO, KEYSDX, MXKYDX)
      CALL DBCKEY (KEYN, KEYSN, MXKYDX)
      CALL CDREPL (PATHN, LSUP, LBK, IUDIV, KEYSDX,KEYSN, CHOPT, IRC)
      IQUEST(1) = IRC
      IF (IRC.EQ.0.AND.IOPCDX.NE.0)  THEN
        IF (LBK(1).NE.0) THEN
          LBD(1) = LQ(KOFUCD+LBK(1)-KLDACD)
        ELSE
          LBD(1) = 0
        ENDIF
      ENDIF
*                                                             END DBREPL
  999 END
