*
* $Id: cdedas.F,v 1.1.1.1 1996/02/28 16:24:49 mclareni Exp $
*
* $Log: cdedas.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:49  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEDAS (CFNAM, NCF, PATHN, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDEDAS (CFNAM, NCF, PATHN, IRC*)                        *
*                                                                      *
*   Routine to insert objects from a pre-edited ascii file             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CFNAM    Character string describing the name of the file used   *
*              for the editing                                         *
*     NCF      Number of characters in the string CFNAM                *
*     PATHN    Character string describing the pathname                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDAUXI                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      CHARACTER       CFNAM*(*), PATHN*(*), STRFL*300, PATHY*80
      CHARACTER       CHOPT*4, YESNO*1
      DIMENSION       KEYS(MXDMCK), KEYO(MXDMCK)
*
*     ------------------------------------------------------------------
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)              GO TO 999
      PATHY  = PAT1CT
      CALL CDKYTG
      CALL CDCONC (IOTYCK(1), NWKYCK, ICONCK(1), NSKPCK)
*
      CALL KUPROC ('Data is to be inserted ? (Y/N) ', YESNO, LYN)
      IF (YESNO.EQ.'Y') THEN
        CHOPT  = 'IAD'
      ELSE
        CHOPT  = 'IAW'
      ENDIF
      CALL CDOPFL (LUKYCX, CFNAM, 'OLD', ISTAT)
      IF (ISTAT.NE.0)            GO TO 999
      CALL KUPROC ('Horizontal or Vertical Mode ? (H/V) ', YESNO, LYN)
      IF (YESNO.EQ.'H') THEN
        CHOPT(4:4) = 'H'
        READ (LUKYCX, 1001, ERR=20, END=20)
        CALL VZERO (KEYO, NWKYCK)
*
   10   CONTINUE
        READ (LUKYCX, CFMTCC, ERR=20, END=20) STRFL(1:MXDPCC)
        IF (STRFL(1:20).EQ.' ') THEN
          GO TO 20
        ELSE IF (STRFL(1:1).EQ.'D') THEN
*
*  **     The object is to be inserted
*
          CALL VZERO (KEYS, NWKYCK)
          CALL CDDCKH (STRFL, KEYS(1), IKEY, IRC)
          IF (IRC.NE.0)          GO TO 20
          KEYS(IDHKSN) = IKEY
          IOPX   = IOPXCA
          CALL CDUPKY (KEYS, KEYO, PATHN, CHOPT, IRC)
          IOPXCA = IOPX
          IF (IRC.NE.0)          GO TO 20
        ENDIF
        GO TO 10
      ELSE
        CHOPT(4:4) = 'V'
        CALL VZERO (KEYS, NWKYCK)
        READ (LUKYCX, 1002, ERR=20, END=20)
   15   CONTINUE
        READ (LUKYCX, '(A80)', ERR=20, END=20) STRFL(1:80)
        IF (STRFL(40:40).NE.'D') GO TO 20
        READ (STRFL(21:30), 1004, ERR=20) KEYS(IDHKSN)
        CALL CDDCKV (LUKYCX, ICONCK(1), KEYS(1), IRC)
        IF (IRC.NE.0)            GO TO 20
        IOPX   = IOPXCA
        CALL CDUPKY (KEYS, KEYO, PATHN,  CHOPT, IRC)
        IOPXCA = IOPX
        IF (IRC.NE.0)            GO TO 20
        GO TO 15
      ENDIF
*
   20 CALL CDCLFL (LUKYCX)
*
 1001 FORMAT (///)
 1002 FORMAT (/)
 1003 FORMAT (A)
 1004 FORMAT (I10)
*                                                             END CDEDAS
 999  END
