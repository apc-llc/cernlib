*
* $Id: dbout.F,v 1.1.1.1 1996/02/28 16:25:01 mclareni Exp $
*
* $Log: dbout.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:01  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBOUT (PATHN, IVSTR, IVEND, LSUP, IPRVS, NKEXT, KEYXT,
     +                  IPREC, CHOPT)
*     =================================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBOUT (PATHN, IVSTR, IVEND, LSUP, IPRVS, NKEXT, KEYXT,  *
*                     IPREC, CHOPT)                                    *
*                                                                      *
*   Stores data from a ZEBRA bank to disk creating simultaneousely     *
*   the directories if needed                                          *
*   Restrictions : Only directories upto 9 keys can be created and     *
*                  keys should be of type INTEGER                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IVSTR    Start of validity period                                *
*     IVEND    End of validity period                                  *
*     LSUP     Address of bank in memory where data reside             *
*     IPRVS    Version number of program used for creating the data    *
*     NKEXT    Number of extra keys (Beyond key 5)                     *
*     KEYXT    Vector of extra keys                                    *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*     CHOPT    Character string with any of the following characters   *
*          F   Updates with a fully matched data object (in user keys) *
*          N   Create new (sub)directory(ies)                          *
*          P   Create partitioned subdirectories for the pathname      *
*          R   Store with full RZ option (No compression to be made)   *
*          S   Create stand alone (master) data                        *
*          U   Store data uncompressed                                 *
*          Z   Store only nonzero elements. An element is considered to*
*              be zero if its absolute value is less than IPREC (real) *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 62 : Too many external keys with option N            *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      DIMENSION       KEYXT(9), LSUP(9)
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
* *** Prepare the Key vector array
*
      CALL VZERO (MASKDX, MXKYDX)
      IF (NKEXT.GT.2) CALL UCOPY (KEYXT(3), MASKDX(8), NKEXT-2)
      CALL DBCKEY (MASKDX, KEYSDX, MXKYDX)
      CALL VZERO (MASKDX, MXKYDX)
      KEYSDX(NOF1CK+1) = IVSTR
      KEYSDX(NOF1CK+2) = IVEND
      KEYSDX(IDHUSI)   = IPRVS
*
      CALL CDSTOR (PATHN, LSUP, LBK, IUDIV, KEYSDX, CHOP, IRC)
      IQUEST(1) = IRC
*                                                              END DBOUT
  999 END
