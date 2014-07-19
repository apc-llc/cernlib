*
* $Id: cdacti.F,v 1.1.1.1 1996/02/28 16:24:46 mclareni Exp $
*
* $Log: cdacti.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:46  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDACTI
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDACTI                                                  *
*                                                                      *
*   Action Routines for menu /HEPDB/INITCLOSE                          *
*                                                                      *
*   Allowed Actions :                                                  *
*                                                                      *
*     CDCRDR, CDDDIR, CDBFOR, CDENFL, CDEND,  CDFZOP, CDFZUP, CDILDF,  *
*     CDILDU, CDINIT, CDLOGL, CDPART, CDSAVE, CDSETD                   *
*                                                                      *
*   Called by KUIP routine                                             *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ccdisp.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/cxlink.inc"
      PARAMETER       (MXLUN=10)
      CHARACTER       CPATL*32, CHOPT*32, TOPNM*32, PATHN*80, CHOP*32
      CHARACTER       PATHI*80, CFNAM*80
      DIMENSION       LUNRZ(MXLUN)
      DATA            PATHN /' '/, TOPNM /' '/, CFNAM /' '/, PATHI /' '/
      DATA            IDATE /800101/, ITIME /0/, LUNF /0/, LUNI /0/
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'CDCRDR') THEN
*
*  **   CDCRDR
*
        CALL KUGETC (PATHN, NCH)
        CALL KUGETI (NKEX)
        CALL KUGETC (CHOPT, NCP)
        CALL CDCRDR (PATHN, NKEX, CHOPT, IRC)
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Create directory for '//
     +       PATHN(1:NCH)//''',/,''          return code '',I12)',
     +       IRC, 1)
*
      ELSE IF (CPATL.EQ.'CDDDIR') THEN
*
*  **   CDDDIR
*
        CALL KUGETC (PATHN, NCH)
        CALL CDDDIR (PATHN, ' ', IRC)
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Delete the directory tre'//
     +       'e '//PATHN(1:NCH)//''',/,''          return code '',I12)',
     +       IRC, 1)
*
      ELSE IF (CPATL.EQ.'CDBFOR') THEN
*
*  **   CDBFOR
*
        CALL KUGETC (TOPNM, NCH)
        CALL KUGETI (IDATE)
        CALL KUGETI (ITIME)
        CALL CDBFOR (TOPNM, IDATE, ITIME, IRC)
        IARGCD(1) = IDATE
        IARGCD(2) = ITIME
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Retrieve data for '//
     +       TOPNM(1:NCH)//' before '',2I8)', IARGCD, 2)
*
      ELSE IF (CPATL.EQ.'CDENFL') THEN
*
*  **   CDENFL
*
        CALL KUGETC (TOPNM, NCH)
        NLUNS  = 0
        LTOP   = LTOPCD
    5   IF (LTOP.GT.0) THEN
          NCHR   = IQ(KOFUCD+LTOP+MUPNCH)
          CALL UHTOC (IQ(KOFUCD+LTOP+MUPNAM), 4, TOPNCI, NCHR)
          TOPNCI = TOPNCI(1:NCHR)
          IF (TOPNCI.EQ.TOPNM) THEN
            NLUNS  = NLUNS + 1
            LUNRZ(NLUNS) = IQ(KOFUCD+LTOP+MUPLUN)
            GO TO 10
          ELSE
            LTOP   = LQ(KOFUCD+LTOP)
            GO TO 5
          ENDIF
        ENDIF
   10   CALL CDEND (TOPNM, ' ', IRC)
        IF (NLUNS.GT.0) THEN
          DO 15 ILUN = 1, NLUNS
            CALL CDCLFL (LUNRZ(ILUN))
   15     CONTINUE
        ENDIF
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Close data base file for'//
     +       ' '//TOPNM//''')', IARGCD, 0)
*
      ELSE IF (CPATL.EQ.'CDEND') THEN
*
*  **   CDEND
*
        NLUNS  = 0
        LTOP   = LTOPCD
   20   IF (LTOP.GT.0.AND.NLUNS.LT.MXLUN) THEN
          NLUNS  = NLUNS + 1
          LUNRZ(NLUNS) = IQ(KOFUCD+LTOP+MUPLUN)
          LTOP   = LQ(KOFUCD+LTOP)
          GO TO 20
        ENDIF
        CALL CDEND ('*', 'A', IRC)
        IF (NLUNS.GT.0) THEN
          DO 25 ILUN = 1, NLUNS
            CALL CDCLFL (LUNRZ(ILUN))
   25     CONTINUE
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDFZOP') THEN
*
*  **   CDFZOP
*
        CALL KUGETC (TOPNM, NCH)
        CALL KUGETI (LUNF)
        CALL KUGETC (CHOPT, NCH)
        CALL CDFZOP (LUNF, TOPNM, CHOPT, IRC)
        IARGCD(1) = LUNF
        IARGCD(2) = IRC
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Open Journal file '',I4'//
     +       ','' for '//TOPNM(1:10)//CHOPT(1:2)//''',/,''          '//
     +       'return code '',I12)', IARGCD, 2)
*
      ELSE IF (CPATL.EQ.'CDFZUP') THEN
*
*  **   CDFZUP
*
        CALL KUGETI (LUNF)
        CALL KUGETC (CHOPT, NCH)
        CALL CDFZUP (LUNF, CHOPT, IRC)
        IARGCD(1) = LUNF
        IARGCD(2) = IRC
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Update from Journal file'//
     +       ' '',I4,'' return code'',I12)', IARGCD, 2)
*
      ELSE IF (CPATL.EQ.'CDILDF') THEN
*
*  **   CDILDF
*
        CALL KUGETI (LUNI)
        CALL KUGETC (CFNAM, NCH)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL CDOPFL (LUNI, CFNAM, 'OLD', ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL KUGETC (CHOPT, NCH)
          CALL CDILDF (LUNI, CHOPT, IRC)
          CALL CDCLFL (LUNI)
          IARGCD(1) = IQUEST(11)
          IARGCD(2) = IRC
          CALL CDPRNT (L3PRCX, '(/,'' CDACTI : list of'',I6,'' direct'//
     +         'ories read from '//CFNAM(1:40)//' error code'',I12)',
     +         IARGCD, 2)
        ELSE
          CALL CDPRNT (L3PRCX, '(/,'' CDACTI : error in opening file '//
     +         CFNAM(1:40)//' on unit '',I6)', LUNI, 1)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDILDU') THEN
*
*  **   CDILDU
*
        CALL KUGETI (LUNI)
        CALL KUGETC (CFNAM, NCH)
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL CDOPFL (LUNI, CFNAM, 'OLD', ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL KUGETC (TOPNM, NCH)
          CALL KUGETC (CHOPT, NCH)
          CALL CDILDU (LUNI, TOPNM, CHOPT, IRC)
          CALL CDCLFL (LUNI)
          IARGCD(1) = IQUEST(11)
          IARGCD(2) = IRC
          CALL CDPRNT (L3PRCX, '(/,'' CDACTI : list of'',I6,'' direct'//
     +         'ories read from '//CFNAM(1:40)//' error code'',I12)',
     +         IARGCD, 2)
        ELSE
          CALL CDPRNT (L3PRCX, '(/,'' CDACTI : error in opening file '//
     +         CFNAM(1:40)//' on unit '',I6)', LUNI, 1)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDINIT') THEN
*
*  **   CDINIT
*
        CALL KUGETI (LUNRZ)
        CALL KUGETC (CFNAM, NCH)
        CALL KUGETC (TOPNM, NCH)
        CALL KUGETI (LRECL)
        CALL KUGETC (CHOP, NCH)
        CALL RZOPEN (LUNRZ, TOPNM, CFNAM, CHOP, LRECL, IRC)
        IF (IRC.NE.0) GO TO 999
        CALL KUGETI (IDIV)
        CALL KUGETI (NREC)
        IF (NREC.GT.0) THEN
          CHOPT  = 'Z'//CHOP
        ELSE
          CHOPT  = CHOP
        ENDIF
        CALL KUGETI (NPAIR)
        CALL KUGETI (NTOP)
        CALL CDINIT (IDIV, LUNRZ,0, TOPNM, NPAIR,NREC, NTOP, CHOPT, IRC)
        IF (IRC.NE.0) THEN
          CALL CDCLFL (LUNRZ)
        ENDIF
*
      ELSE IF (CPATL.EQ.'CDLOGL') THEN
*
*  **   CDLOGL
*
        CALL KUGETC (TOPNM, NCH)
        CALL KUGETI (LOGL)
        CALL KUGETC (CHOP, NCH)
        CALL CDLOGL (TOPNM, LOGL, CHOPT, IRC)
*
      ELSE IF (CPATL.EQ.'CDPART') THEN
*
*  **   CDPART
*
        CALL KUGETC (PATHI, NCHI)
        CALL KUGETC (PATHN, NCHO)
        CALL KUGETI (MXKP)
        CALL KUGETC (CHOPT, NCH)
        CALL CDPART (PATHI, PATHN, MXKP, CHOPT, IRC)
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Copies '//PATHI(1:NCHI)//
     +       ' to '',/,''                 '//PATHN(1:NCHO)//' return '//
     +       'code '',I12)', IRC, 1)
*
      ELSE IF (CPATL.EQ.'CDSAVE') THEN
*
*  **   CDSAVE
*
#if (defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX))&&(defined(CERNLIB__SERVER))&&(!defined(CERNLIB__P3CHILD))&&(!defined(CERNLIB__ONLINE))
        CALL CDSTSV (' ', 0, IRC)
#endif
*       CALL CDSAVE (IRC)
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : Saves the changes to '//
     +       'data base - return code'',I12)', IRC, 1)
*
      ELSE IF (CPATL.EQ.'CDSETD') THEN
*
*  **   CDSETD
*
        CALL KUGETI (MXDIS)
        IF (MXDIS.LE.0) THEN
          MXDPCC = 80
        ELSE IF (MXDIS.GT.512) THEN
          MXDPCC = 512
        ELSE
          MXDPCC = MXDIS
        ENDIF
        WRITE (CFMTCC, '(''(A'',I3,'')'')') MXDPCC
        CALL CDPRNT (L3PRCX, '(/,'' CDACTI : MXDIS is set to '',I10)',
     +       MXDPCC, 1)
*
      ENDIF
*                                                             END CDACTI
  999 END
