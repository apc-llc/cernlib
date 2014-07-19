*
* $Id: dbdont.F,v 1.1.1.1 1996/02/28 16:24:54 mclareni Exp $
*
* $Log: dbdont.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:54  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBDONT (PATHN, IUDIV, LSUP, NWKEY, KEY, CHOPT)
*     =========================================================
*
************************************************************************
*                                                                      *
*        SUBR. DBDONT (PATHN, IUDIV, LSUP, NWKEY, KEY, CHOPT)          *
*                                                                      *
*   Stores data from memory to disk. If data exist with the same user  *
*   keys, the old object is physically replaced. This mode is not      *
*   recommended for all calibration data.                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IUDIV    Division index of the user data bank                    *
*     LSUP     Address of bank in memory where data reside             *
*     NWKEY    Number of keys associated with the data bank            *
*     KEY      Vector of keys                                          *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          7   Insertion time as supplied by user to be honoured       *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*               = 61 : Too many keys                                   *
*               = 71 : Illegal path name                               *
*               = 73 : RZ error during writing to disk                 *
*               = 75 : Cannot find the Top directory name in pathname  *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "dxused.inc"
      DIMENSION       KEY(9), LSUP(9)
      CHARACTER       CHOPT*(*), PATHN*(*), CHOPF*28
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
      IF (IOKYDX(7).NE.0) IOPHDX = 1
      CALL DBOPTM (CHOPF)
      CALL DBCKEY (KEY, KEYSDX, MXKYDX)
*
      CALL CDDONT (PATHN, IUDIV, LSUP, KEYSDX, CHOPF, IRC)
      IQUEST(1) = IRC
*                                                             END DBDONT
  999 END
