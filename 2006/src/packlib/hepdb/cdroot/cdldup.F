*
* $Id: cdldup.F,v 1.1.1.1 1996/02/28 16:24:29 mclareni Exp $
*
* $Log: cdldup.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:29  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLDUP (PATHN, IFLG, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDLDUP (PATHN, IFLG, IRC*)                              *
*                                                                      *
*   Sets current directory to PATHN and loads /CDUPDB/ with information*
*   about the top directory                                            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     IFLG     Flag (< 0 if current directory is already set; then     *
*              PATHN is ignored)                                       *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDDONT, CDENTB, CDKOUT, CDMDIR, CDNODE, CDPART, CDPRIN,  *
*             CDRENK, CDRTFZ, CDUSE,  CDUSEM, CDUSEDB                  *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 96 : RZCDIR fails to set to the current directory    *
*               = 97 : The corresponding UPCD bank not found           *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*)
*
*     ------------------------------------------------------------------
*
* *** Remove blanks in the path name and set current directory
*
      IF (IFLG.GE.0) THEN
        CALL CDLDDI (PATHN, PAT1CT, NCH)
        CALL RZCDIR (PAT1CT, ' ')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 96
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) THEN
            IF (NCH.EQ.0) NCH = MAXLCD
            CALL CDPRNT (LPRTCD, '(/,'' CDLDUP : Cannot set current '//
     +           'directory to '//PAT1CT(1:NCH)//' '')', IARGCD, 0)
          ENDIF
#endif
          GO TO 999
        ENDIF
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
      ENDIF
*
* *** Now get the top directory name
*
      CALL RZCDIR (PAT1CT, 'R')
      CALL CDTOPN (PAT1CT, TOPNCD, NCHRCD)
*
* *** Load the UPDB bank content for this top directory
*
      IPRBCA = 0
      IPRECA = 0
      LBUPCD = LTOPCD
   10 IF (LBUPCD.NE.0) THEN
        NCHR   = IQ(KOFUCD+LBUPCD+MUPNCH)
        CALL UHTOC (IQ(KOFUCD+LBUPCD+MUPNAM), 4, TOP1CT, NCHR)
        IF (TOP1CT(1:NCHR).NE.TOPNCD) THEN
          LBUPCD  = LQ(KOFUCD+LBUPCD)
          GO TO 10
        ELSE
          LURZCD = IQ(KOFUCD+LBUPCD+MUPLUN)
          IOUTCD = IQ(KOFUCD+LBUPCD+MUPFLG)
          IOPPCD = IQ(KOFUCD+LBUPCD+MUPSRV)
          IOPSCD = IQ(KOFUCD+LBUPCD+MUPSHR)
          NPARCD = IQ(KOFUCD+LBUPCD+MUPAIR)
          ITOPCD = IQ(KOFUCD+LBUPCD+MUPDIC)
          MXINCD = IQ(KOFUCD+LBUPCD+MUPKIN)
          LUFZCD = IQ(KOFUCD+LBUPCD+MUPJFL)
          LUBKCD = IQ(KOFUCD+LBUPCD+MUPBAK)
          IHFLCD = IQ(KOFUCD+LBUPCD+MUPHFL)
          IRC    = 0
*
          NSYSCK = NOF2CK + 2*NPARCD
          DO 15 I = 1, NPARCD
            IF (IOKYCA(NOF1CK+2*I-1).NE.0) IPRBCA = IPRBCA + 1
            IF (IOKYCA(NOF1CK+2*I)  .NE.0) IPRECA = IPRECA + 1
   15     CONTINUE
        ENDIF
*
      ELSE
        IRC    = 97
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          CALL CDPRNT (LPRTCD, '(/,'' CDLDUP : Cannot find the UPCD '//
     +         'bank for '//TOPNCD(1:NCHRCD)//' in the path '//PAT1CT//
     +         ' '')', IARGCD, 0)
        ENDIF
#endif
        GO TO 999
      ENDIF
*                                                             END CDLDUP
  999 END
