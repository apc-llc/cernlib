*
* $Id: cdaird.F,v 1.1.1.1 1996/02/28 16:24:46 mclareni Exp $
*
* $Log: cdaird.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDAIRD (LUN, PATHN, KEYX, CHOPT, IRC)
*     ================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDAIRD (LUN, PATHN, KEYX, CHOPT, IRC*)                  *
*                                                                      *
*   Display the data attached to the directory PATHN and KEY for       *
*   ASCII type of data                                                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit number of file for display                         *
*     PATHN    Character string describing the pathname                *
*     KEYX     Key-vector                                              *
*     CHOPT    Character option                                        *
*          E   Object already exists at LFRSCX                         *
*          L   The last object to be shown                             *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDAIWR, CDPEEK                                           *
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
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       KEYX(9), IOPTS(2)
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80
      EQUIVALENCE     (IOPTS(1), IOPTE), (IOPTS(2), IOPTL)
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      CALL UOPTC (CHOPT, 'EL', IOPTS)
      IF (IOPTE.NE.0)                            GO TO 10
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                              GO TO 999
      PATHY  = PAT1CT
      CALL CDKEYT
      IF (NKEYCK.LE.0)                           GO TO 20
*
*  ** Find the appropriate object number
*
      IF (IOPTL.NE.0) THEN
        IOPK   = IOPKCA
        CALL CDLKEY (PATHY, KEYVCK, IDATE, ITIME, 'K', IRC)
        IOPKCA = IOPK
        IF (IRC.NE.0)                            GO TO 999
        KYSER  = KEYVCK(IDHKSN)
      ELSE
        KYSER  = KEYX(IDHKSN)
      ENDIF
      CALL CDSGET (KYSER, IDIVCD, LFRSCX, IRC)
      IF (IRC.NE.0)                              GO TO 999
   10 IF (LFRSCX.EQ.0)                           GO TO 20
      NDATA  = IQ(KOFUCD+LFRSCX-1)
      IF (NDATA.LE.0)                            GO TO 20
*
*  ** Display data if exists, if it does not, display dummy data
*
      CALL CDAFRI (LUN, LFRSCX, ' ', IRC)
      GO TO 30
*
   20 WRITE (LUN, 1001) 'There is no data '
   30 IF (IOPTE.EQ.0) THEN
        IF (LFRSCX.NE.0) CALL MZDROP (IDIVCD, LFRSCX, 'L')
      ENDIF
      IRC    = 0
*
 1001 FORMAT (A)
*                                                             END CDAIRD
  999 END
