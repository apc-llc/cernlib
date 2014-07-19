*
* $Id: cdacpl.F,v 1.1.1.1 1996/02/28 16:24:45 mclareni Exp $
*
* $Log: cdacpl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDACPL
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDACPL                                                  *
*                                                                      *
*   Action Routines for menu /HEPDB/PLOT                               *
*                                                                      *
*   Allowed Actions :                                                  *
*                                                                      *
*     CDHELP, CDPLOB, CDPLOT, CDPLOV, CDPLTI, CDREAD, CDTREE           *
*                                                                      *
*   Called by KUIP routine                                             *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cplots.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      PARAMETER       (L3WKST=1)
      DIMENSION       KOBJ1(NOBMCP), KOBJ2(2,NOBMCP)
      CHARACTER       CPATL*32, CHOPT*32, PATHN*80, TOPN*16
      CHARACTER       CFNAM*80, ALIAS*8, CHTAG*8
      DATA            PATHN /' '/, ALIAS /' '/, CHTAG /' '/
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'CDHELP') THEN
*
*  **   CDHELP
*
        TOPN   = '*'
        CALL KUGETC (TOPN, NCH)
        IF (LTOPCD.NE.0) THEN
          IF (TOPN(1:1).EQ.'*') THEN
            NCHR   = IQ(KOFUCD+LTOPCD+MUPNCH)
            CALL UHTOC (IQ(KOFUCD+LTOPCD+MUPNAM), 4, TOPN, NCHR)
            TOPN   = TOPN(1:NCHR)
          ENDIF
          PATHN  = '//'//TOPN
          CALL CDFPAT (PATHN, 20, LFRSCX, IRC)
          IF (IRC.EQ.0.AND.LFRSCX.NE.0) THEN
            CALL IGSG (L3WKST)
            CALL ICLRWK (1, 1)
            CALL CDPLBK (LFRSCX, IRC0)
            CALL IRQLC (1, 1, IST, NT, X, Y)
            CALL IGSA (L3WKST)
            IF (IST.NE.0) THEN
              CALL CDLOOK (LFRSCX, X, Y, PATHN)
              CALL MZDROP (IDIVCD, LFRSCX, ' ')
              LFRSCX = 0
              IF (PATHN(1:1).NE.' ') THEN
                CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
                CALL CUTOL  (CFNAM)
#endif
                CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', ISTAT)
                IF (ISTAT.NE.0) THEN
                  CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12'//
     +                 ','' in opening file '//CFNAM(1:NCF)//''')',
     +                 ISTAT, 1)
                  GO TO 999
                ENDIF
                CALL CDRHLP (PATHN, LUKYCX, IERR)
                IF (IERR.NE.0) CALL CDPRNT (LUKYCX, '(/,'' CDACPL : '//
     +          'Error '',I12,'' in getting help info. for '',/,'' '//
     +          PATHN(1:72)//''')', IERR, 1)
                CALL CDCLFL (LUKYCX)
                CALL KUEDIT (CFNAM, IST)
              ELSE
                CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Cursor does not '//
     +               'point to any valid path'')', IARGCD, 0)
              ENDIF
            ELSE
              CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12,'' in'//
     +             ' picking operation'')', IST, 1)
            ENDIF
          ELSE
            CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12,'' in '//
     +           'finding the tree for '//TOPN//''')', IRC, 1)
          ENDIF
          IF (LFRSCX.NE.0) THEN
            CALL MZDROP (IDIVCD, LFRSCX, ' ')
            LFRSCX = 0
          ENDIF
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDPLOB') THEN
*
*  **   CDPLOB
*
        CALL KUGETC (PATHN, NCH)
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in path name '//
     +         PATHN(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        PATHN  = PAT1CT
        CALL KUGETI (NOBJ)
        IF (NOBJ.LT.1) THEN
          NOBJ = 1
        ELSE IF (NOBJ.GT.NOBMCP) THEN
          NOBJ = NOBMCP
        ENDIF
        CALL KUGETI (NMASK)
        CALL KUGETI (KEX)
        CALL KUGETI (NST)
        CALL KUGETC (CHOPT, NCH)
        CALL CDOPTS (CHOPT, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in decoding CHOP'//
     +         'T '//CHOPT(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL VZERO (KEYSCX, 100)
        CALL VZERO (MASKCX, 100)
        CALL CDRVPL (1, NOBJ, NMASK, KOBJ1, KOBJ2, MASKCX, KEYSCX, IRC)
        IF (IRC.NE.0) GO TO 999
        CALL IGSG (L3WKST)
        CALL CDPLOB (PATHN, MASKCX,KEYSCX,NOBJ,KOBJ1,KEX,NST,CHOPT,IRC)
        IF (IRC.NE.0) CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '','//
     +  'I12,'' in routine CDPLOB'')', IRC, 1)
        CALL IGSA (L3WKST)
*
      ELSE IF (CPATL(1:6).EQ.'CDPLOT') THEN
*
*  **   CDPLOT
*
        CALL KUGETC (ALIAS, NCH)
        CALL CDRALI (ALIAS, PATHN, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in the alias nam'//
     +         'e '//ALIAS//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL KUGETC (CHTAG, NCH)
        CALL CDGNAM (PATHN, CHTAG, IOBJ, IRC)
        IF (IRC.NE.0.OR.IOBJ.EQ.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in the object '//
     +         'name '//CHTAG//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in path name '//
     +         PATHN(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        PATHN  = PAT1CT
        CALL KUGETI (NMASK)
        CALL KUGETC (CHOPT, NCH)
        CALL CDOPTS (CHOPT, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in decoding CHOP'//
     +         'T '//CHOPT(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL VZERO (KEYSCX, 100)
        CALL VZERO (MASKCX, 100)
        CALL CDRVPL (0, NOBJ, NMASK, KOBJ1, KOBJ2, MASKCX, KEYSCX, IRC)
        IF (IRC.NE.0) GO TO 999
        NOBJ   = 1
        KEX    = 3
        NST    = 1
        CALL IGSG (L3WKST)
        CALL CDPLOB (PATHN, MASKCX,KEYSCX, NOBJ,IOBJ,KEX,NST, CHOPT,IRC)
        IF (IRC.NE.0) CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '','//
     +  'I12,'' in routine CDPLOB'')', IRC, 1)
        CALL IGSA (L3WKST)
*
      ELSE IF (CPATL.EQ.'CDPLOV') THEN
*
*  **   CDPLOV
*
        CALL KUGETC (PATHN, NCH)
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in path name '//
     +         PATHN(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        PATHN  = PAT1CT
        CALL KUGETI (NOBJ)
        IF (NOBJ.LT.1) THEN
          NOBJ = 1
        ELSE IF (NOBJ.GT.NOBMCP) THEN
          NOBJ = NOBMCP
        ENDIF
        CALL KUGETI (NMASK)
        CALL KUGETI (NST)
        CALL KUGETC (CHOPT, NCH)
        CALL CDOPTS (CHOPT, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in decoding CHOP'//
     +         'T '//CHOPT(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL VZERO (KEYSCX, 100)
        CALL VZERO (MASKCX, 100)
        CALL CDRVPL (2, NOBJ, NMASK, KOBJ1, KOBJ2, MASKCX, KEYSCX, IRC)
        IF (IRC.NE.0) GO TO 999
        CALL IGSG (L3WKST)
        CALL CDPLOV (PATHN, MASKCX,KEYSCX, NOBJ,KOBJ2, NST, CHOPT, IRC)
        IF (IRC.NE.0) CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '','//
     +  'I12,'' in routine CDPLOV'')', IRC, 1)
        CALL IGSA (L3WKST)
*
      ELSE IF (CPATL.EQ.'CDPLTI') THEN
*
*  **   CDPLTI
*
        CALL KUGETC (PATHN, NCH)
        CALL CDLDUP (PATHN, 0, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in path name '//
     +         PATHN(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        PATHN  = PAT1CT
        CALL KUGETI (NMASK)
        CALL KUGETC (CHOPT, NCH)
        CALL CDOPTS (CHOPT, IRC)
        IF (IRC.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error in decoding CHOP'//
     +         'T '//CHOPT(1:NCH)//''')', IARGCD, 0)
          GO TO 999
        ENDIF
        CALL VZERO (KEYSCX, 100)
        CALL VZERO (MASKCX, 100)
        CALL CDRVPL (0, NOBJ, NMASK, KOBJ1, KOBJ2, MASKCX, KEYSCX, IRC)
        IF (IRC.NE.0) GO TO 999
        CALL IGSG (L3WKST)
        CALL CDPLTI (PATHN, MASKCX, KEYSCX, CHOPT, IRC)
        IF (IRC.NE.0) CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '','//
     +  'I12,'' in routine CDPLTI'')', IRC, 1)
        CALL IGSA (L3WKST)
*
      ELSE IF (CPATL.EQ.'CDREAD') THEN
*
*  **   CDREAD
*
        TOPN   = '*'
        CALL KUGETC (TOPN, NCH)
        IF (LTOPCD.NE.0) THEN
          IF (TOPN(1:1).EQ.'*') THEN
            NCHR   = IQ(KOFUCD+LTOPCD+MUPNCH)
            CALL UHTOC (IQ(KOFUCD+LTOPCD+MUPNAM), 4, TOPN, NCHR)
            TOPN   = TOPN(1:NCHR)
          ENDIF
          PATHN  = '//'//TOPN
          CALL CDFPAT (PATHN, 20, LFRSCX, IRC)
          IF (IRC.EQ.0.AND.LFRSCX.NE.0) THEN
            CALL IGSG (L3WKST)
            CALL ICLRWK (1, 1)
            CALL CDPLBK (LFRSCX, IRC0)
            CALL IRQLC (1, 1, IST, NT, X, Y)
            CALL IGSA (L3WKST)
            IF (IST.NE.0) THEN
              CALL CDLOOK (LFRSCX, X, Y, PATHN)
              CALL MZDROP (IDIVCD, LFRSCX, ' ')
              LFRSCX = 0
              IF (PATHN(1:1).NE.' ') THEN
                CALL KUGETC (CFNAM, NCF)
#if defined(CERNLIB_UNIX)
                CALL CUTOL  (CFNAM)
#endif
                CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', ISTAT)
                IF (ISTAT.NE.0) THEN
                  CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12'//
     +                 ','' in opening file '//CFNAM(1:NCF)//''')',
     +                 ISTAT, 1)
                  GO TO 999
                ENDIF
                CALL KUGETC (CHOPT, NCH)
                CALL UOPTC  (CHOPT, 'H', IOPHCC)
                CALL UOPTC  (CHOPT, 'X', IOPXCA)
                CALL CDPEEK (CFNAM, NCF, PATHN, IRC)
              ELSE
                CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Cursor does not '//
     +               'point to any valid path'')', IARGCD, 0)
              ENDIF
            ELSE
              CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12,'' in'//
     +             ' picking operation'')', IST, 1)
            ENDIF
          ELSE
            CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12,'' in '//
     +           'finding the tree for '//TOPN//''')', IRC, 1)
          ENDIF
          IF (LFRSCX.NE.0) THEN
            CALL MZDROP (IDIVCD, LFRSCX, ' ')
            LFRSCX = 0
          ENDIF
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDTREE') THEN
*
*  **   CDTREE
*
        TOPN   = '*'
        CALL KUGETC (TOPN, NCH)
        IF (LTOPCD.NE.0) THEN
          IF (TOPN(1:1).EQ.'*') THEN
            NCHR   = IQ(KOFUCD+LTOPCD+MUPNCH)
            CALL UHTOC (IQ(KOFUCD+LTOPCD+MUPNAM), 4, TOPN, NCHR)
            TOPN   = TOPN(1:NCHR)
          ENDIF
          PATHN  = '//'//TOPN
          CALL CDFPAT (PATHN, 20, LFRSCX, IRC)
          IF (IRC.EQ.0.AND.LFRSCX.NE.0) THEN
            CALL IGSG (L3WKST)
            CALL ICLRWK (1, 1)
            CALL CDPLBK (LFRSCX, IRC0)
            CALL IGSA (L3WKST)
          ELSE
            CALL CDPRNT (L3PRCX, '(/,'' CDACPL : Error '',I12,'' in '//
     +           'finding the tree for '//TOPN//''')', IRC, 1)
          ENDIF
          IF (LFRSCX.NE.0) THEN
            CALL MZDROP (IDIVCD, LFRSCX, ' ')
            LFRSCX = 0
          ENDIF
        ENDIF
*
      ENDIF
*                                                             END CDACPL
  999 END
