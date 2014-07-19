*
* $Id: dbvout.F,v 1.1.1.1 1996/02/28 16:25:03 mclareni Exp $
*
* $Log: dbvout.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:03  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBVOUT (PATHN, IVSTR, IVEND, NDAT, USER, IPRVS, NKEXT,
     +                   KEYXT, IDTYP, IPREC, CHOPT)
*     =================================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBVOUT (PATHN, IVSTR, IVEND, NDAT, USER, IPRVS, NKEXT,  *
*                      KEYXT, IDTYP, IPREC, CHOPT)                     *
*                                                                      *
*   Stores data from a FORTRAN array to disk creating simultaneousely  *
*   the directories if needed                                          *
*   Restrictions : Only directories upto 9 keys can be created and     *
*                  keys should be of type INTEGER                      *
*                  USER should contain variables of the same type      *
*                  (Integer, Real or Holllereith)                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IVSTR    Start of validity period                                *
*     IVEND    End of validity period                                  *
*     NDAT     Number of data word in the user array                   *
*     USER     Array of user data words                                *
*     IPRVS    Version number of program used for creating the data    *
*     NKEXT    Number of extra keys (Beyond key 5)                     *
*     KEYXT    Vector of extra keys                                    *
*     IDTYP    Type of data (2 - Integer; 3 - Real; 5 - Hollerith)     *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*     CHOPT    Character string with any of the following characters   *
*          F   Updates with a fully matched data object (in user keys) *
*          N   Create new (sub)directory(ies)                          *
*          P   Create partitioned subdirectories for the pathname      *
*          S   Create stand alone (master) data                        *
*          U   Store data uncompressed                                 *
*          Z   Store only nonzero elements. An element is considered to*
*              be zero if its absolute value is less than IPREC (real) *
*                                                                      *
*   Called by user,   DBINIT                                           *
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
#include "hepdb/clinks.inc"
#include "dxused.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       KEYXT(9), USER(2)
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
* *** Create a temporary bank with the data
*
      CALL CDBANK (IDISCD, LAUXCL(7), LAUXCL(7), JBIAS, 'AUX7', 0, 0
     +           , NDAT, IDTYP, 0, IRC)
      IF (IRC.NE.0)   GO TO 999
      CALL UCOPY (USER(1), Q(KOFUCD+LAUXCL(7)+1), NDAT)
*
* *** Write out the data
*
      CALL CDSTOR (PATHN, LSUP, LBK, IUDIV, KEYSDX, CHOP, IRC)
      CALL MZDROP (IDISCD, LAUXCL(7), ' ')
      IQUEST(1) = IRC
*                                                             END DBVOUT
  999 END
