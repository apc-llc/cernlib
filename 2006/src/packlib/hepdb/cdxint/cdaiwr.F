*
* $Id: cdaiwr.F,v 1.1.1.1 1996/02/28 16:24:46 mclareni Exp $
*
* $Log: cdaiwr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDAIWR (KEYX, KEYO, PATHN, CHOPT, IRC)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDAIWR (KEYX, KEYO, PATHN, CHOPT, IRC)                  *
*                                                                      *
*   Insert/Update ASCII files in the current working directory         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYX     Key-Vector containing the new Key values                *
*     KEYO     Key-Vector containing the old Key values                *
*     PATHN    Pathname of the directory                               *
*     CHOPT    Character options                                       *
*         I    Insert new Key                                          *
*         X    Replace (update) old key                                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDEDKY                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       IOPTS(2), KEYX(9), KEYO(9)
      CHARACTER       PATHN*(*), CHOPT*(*), CFNAM*80, CHOPF*4
      EQUIVALENCE     (IOPTI, IOPTS(1)), (IOPTX, IOPTS(2))
*
*     ------------------------------------------------------------------
*
*  ** Open the file to editing
*
      IRC    = 0
      CALL KUPROC ('Give Data File-Name (e.g. DFNAME) ',CFNAM, LFNAM)
#if !defined(CERNLIB_IBMVM)
      CFNAM  = CFNAM(1:LFNAM)//'.FILEXT'
#endif
#if defined(CERNLIB_UNIX)
      CALL CUTOL  (CFNAM)
#endif
#if defined(CERNLIB_IBMVM)
      CFNAM  = CFNAM(1:LFNAM)//'.FILEXT.A'
#endif
      CALL CDOPFL (LUDACX, CFNAM, 'UNKNOWN', ISTAT)
      IF (ISTAT.NE.0)     GO TO 999
*
* *** Decode the character option
*
      CALL UOPTC (CHOPT, 'IX', IOPTS)
      IF (IOPTX.NE.0) THEN
        CALL CDAIRD (LUDACX, PATHN, KEYO, ' ', IRC)
      ELSE
        CALL CDAIRD (LUDACX, PATHN, KEYX, 'L', IRC)
      ENDIF
      CALL CDCLFL (LUDACX)
      IF (IRC.NE.0)       GO TO 999
*
*  ** Edits the file containing the data
*
      CALL KUEDIT (CFNAM, IST)
      IF (IST.NE.0)       GO TO 999
*
*  ** Read the data part
*
      CALL CDOPFL (LUDACX, CFNAM, 'OLD', ISTAT)
      IF (ISTAT.NE.0)     GO TO 999
      CALL CDATOI (LUDACX, LFRSCX, IRC)
      CALL CDCLFL (LUDACX)
      IF (IRC.NE.0)       GO TO 999
*
      IF (IOPTX.NE.0) THEN
        CHOPF  = 'RTY'
      ELSE
        CHOPF  = 'TY'
      ENDIF
      CALL CDSTOM (PATHN, LFRSCX, LKVWCX(1), IDIVCD, NWKYCK, 1, KEYO,
     +             KEYX, CHOPF, IRC)
*                                                             END CDAIWR
  999 END
