*
* $Id: dbentb.F,v 1.1.1.1 1996/02/28 16:24:56 mclareni Exp $
*
* $Log: dbentb.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:56  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBENTB (PATHN, IUDIV, LSUP, KEY, NWDIM, NTKEY, IPREC,
     +                   CHOPT)
*     ================================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBENTB (PATHN, IUDIV, LSUP, *KEY*, NWDIM, NTKEY, IPREC, *
*                      CHOPT)                                          *
*                                                                      *
*   Stores data from memory to disk for a number of objects in a go.   *
*   It is useful in a Batch operation to save real time spent          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IUDIV    Division index where the data reside                    *
*     LSUP     Vector containing the addresses of the banks where      *
*              data reside                                             *
*     KEY      Two dimensional array with the first dimension NWDIM,   *
*              specifying the key elements for each object and the     *
*              secod dimension NTKEY, specifying the number of objects *
*              (Keys 3,4,5 and 8 onwards to be filled in by user on    *
*              input; the DB system keys will be filled in here at the *
*              time of output)                                         *
*     NWDIM    First dimension of the array KEY                        *
*     NTKEY    Number of objects to be inserted                        *
*     IPREC    Precision word; (If IPREC > 0, data are stored with     *
*              IPREC significant digits right to the decimal points; if*
*              IPREC < 0, data are stored with IPREC insignificant     *
*              digits left to the decimal point.)                      *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          F   Updates with a fully matched data object (in user keys) *
*          R   Store with full RZ option (No compression to be made)   *
*          S   Create stand alone (master) data                        *
*          T   Special text type of data (to be used with R)           *
*          U   Store data uncompressed                                 *
*          Z   Store only nonzero elements. An element is considered to*
*              be zero if its absolute is less than IPREC              *
*          7   Insertion time as supplied by user to be honoured       *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 61 : Too many keys                                   *
*               = 65 : Illegal number of objects                       *
*               = 71 : Illegal path name                               *
*               = 72 : Number of keys does not match with that         *
*                      specified in the directory                      *
*               = 73 : RZOUT fails to write on disk                    *
*               = 74 : Error in RZRENK in updating key values for      *
*                      partitioned data set                            *
*               = 75 : Cannot find the Top directory name in pathname  *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*               = 78 : Illegal number of keys in data base             *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "dxused.inc"
      PARAMETER       (NWKYM=30, NOBJM=500)
      DIMENSION       KEY(NWDIM,2), LSUP(9), KEYN(NWKYM,NOBJM)
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
      IOPCDX = 0
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
      IF (NWDIM+NOF2CK+2.GT.NWKYM+NSYSDX.OR.NTKEY.GT.NOBJM) THEN
        DO 10 I = 1, NTKEY
          CALL DBCKEY (KEY(1,I), KEYSDX, MXKYDX)
          LKEYDX = LSUP(I)
          CALL CDSTOR (PATHN, LKEYDX, LBK, IUDIV, KEYSDX, CHOP, IRC)
          IQUEST(1) = IRC
          IF (IQUEST(1).NE.0) GO TO 999
   10   CONTINUE
      ELSE
        DO 20 I = 1, NTKEY
          CALL DBCKEY (KEY(1,I), KEYN(1,I), NWKYM)
   20   CONTINUE
        CALL CDENTB (PATHN, LSUP, IUDIV, NWKYM, NTKEY, KEYN, CHOP, IRC)
        IQUEST(1) = IRC
      ENDIF
*                                                             END DBENTB
  999 END
