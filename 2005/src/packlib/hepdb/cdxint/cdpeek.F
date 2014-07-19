*
* $Id: cdpeek.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdpeek.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPEEK (CFNAM, NCF, PATHN, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPEEK (CFNAM, NCF, PATHN, IRC*)                        *
*                                                                      *
*   Displays keys and data in an interactive session                   *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CFNAM    Character string describing the name of the file used   *
*     NCF      Number of characters in the string CFNAM                *
*     PATHN    Character string describing the pathname                *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDACPL, CDAUXI                                           *
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
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/cxlink.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER       CFNAM*(*), PATHN*(*), CHOP1*1, CHSLA*100, STRFL*40
      CHARACTER       DFNAM*20
      DIMENSION       KEYX(MXDMCK), ITIME(MXPACD)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Display the keys
*
      IF (IOPHCC.NE.0) THEN
        IMRK   = 1
        I1     = 2
        I2     = 9
        CHOP1  = 'K'
      ELSE
        IMRK   = 40
        I1     = 23
        I2     = 30
        CHOP1  = 'U'
      ENDIF
      CALL CDDISP (LUKYCX, PATHN, CHOP1, IRC)
      CALL CDCLFL (LUKYCX)
      IF (IRC.NE.0) THEN
        CHSLA  = PATHN
        NCH    = LENOCC (CHSLA)
        CALL CDPRNT (LPRTCD, '(/,'' CDPEEK : Error '',I6,'' from CDDI'//
     +       'SP for '//CHSLA(1:NCH)//''')', IERR, 1)
        GO TO 999
      ENDIF
*
* *** Open the file for editing
* *** If Data are to be displayed - replace 'D' by an asterix
*
      CALL KUEDIT (CFNAM, IST)
      IF (IST.NE.0) THEN
        CHSLA  = CFNAM
        NCH    = LENOCC (CHSLA)
        IF (IST.NE.1)
     +    CALL CDPRNT (LPRTCD, '(/,'' CDPEEK : Error '',I12,'' in edi'//
     +         'ting file '//CHSLA(1:NCH)//''')', IST, 1)
        GO TO 999
      ENDIF
#if !defined(CERNLIB_IBMVM)
      DFNAM  = 'DFNAME.FILEXT'
#endif
#if defined(CERNLIB_UNIX)
      CALL CUTOL (DFNAM)
#endif
#if defined(CERNLIB_IBMVM)
      DFNAM  = 'DFNAME.FILEXT.A1'
#endif
*
* *** Read back the edited file -- look for the '*'
*
      CALL CDOPFL (LUKYCX, CFNAM, 'OLD', ISTAT)
      IF (ISTAT.NE.0) THEN
        CHSLA  = CFNAM
        NCH    = LENOCC (CHSLA)
        CALL CDPRNT (LPRTCD, '(/,'' CDPEEK : Error '',I12,'' in open'//
     +       'ing file '//CHSLA(1:NCH)//''')', ISTAT, 1)
        GO TO 999
      ENDIF
      CALL UCOPY (IOTYCK, IOTYCC, NWKYCK)
      IF (IOPXCA.NE.0) THEN
        DO 5 I = 1, 2*NPARCD
          IOTYCC(NOF1CK+I) = 7
    5   CONTINUE
      ENDIF
      IOTYCC(IDHINS) = 8
      CHSLA  = ' '
      IF (IOPHCC.NE.0) THEN
        READ (LUKYCX, 1001, ERR=30, END=30)
        NINCR  = 0
      ELSE
        READ (LUKYCX, 1002, ERR=30, END=30)
        NINCR  = NWKYCK - NSKPCK - 1
        CHSLA(1:1) = '('
        CHSLA(NINCR+2:NINCR+2) = ')'
        DO 10 I = 1, NINCR
   10   CHSLA(I+1:I+1) = '/'
      ENDIF
      DO 15 I = 1, NPARCD
        ITIME(I) = 1
   15 CONTINUE
*
   20 CONTINUE
      READ (LUKYCX, 1003, ERR=30, END=30) STRFL
      IF ((IOPHCC.NE.0) .AND. (STRFL(1:20).EQ.' ')) THEN
        GO TO 30
      ELSE IF ((IOPHCC.EQ.0) .AND. (STRFL(IMRK:IMRK).EQ.'D')) THEN
        READ (LUKYCX, CHSLA(1:NINCR+2), ERR=30, END=30)
        GO TO 20
      ELSE IF (STRFL(IMRK:IMRK).EQ.'*') THEN
*
*  **   Fetch the data according to the key vector
*
        READ (STRFL(I1:I2), 1004, ERR=30) KEYX(IDHKSN)
        IOLD   = IOKYCA(IDHKSN)
        IOKYCA(IDHKSN) = 1
        CALL CDKXIN (ITIME, IDIVCD, LFRSCX, LFRSCX, JBIAS, NWKEY, KEYX,
     +               IPREC, IRC)
        IOKYCA(IDHKSN) = IOLD
        IF (IRC.EQ.0.AND.IQ(KOFUCD+LFRSCX-1).GT.0) THEN
*
*   *     Display the data
*
          CALL CDOPFL (LUDACX, DFNAM, 'UNKNOWN', IST1)
          IF (IST1.NE.0) THEN
            CHSLA  = DFNAM
            NCH    = LENOCC (CHSLA)
            CALL CDPRNT (LPRTCD, '(/,'' CDPEEK : Error '',I12,'' in '//
     +           'opening file '//CHSLA(1:NCH)//''')', IST1, 1)
            GO TO 30
          ENDIF
          IF (JBIT(KEYX(IDHFLG), JASFCD).EQ.0) THEN
            CALL CDDKYV (LUDACX, NWKYCK, CTAGCK, KEYX, IOTYCC, ICONCK,
     +                   IRC)
            CALL CDWRDP (LUDACX, LFRSCX)
          ELSE
            CALL UCOPY  (KEYX, KEYVCK, NWKEY)
            CALL CDAIRD (LUDACX, PATHN, KEYX, 'E', IRC)
          ENDIF
          CALL CDCLFL (LUDACX)
          CALL KUEDIT (DFNAM, IST2)
        ELSE
          IF (IRC.NE.0) THEN
            IARGCD(1) = IRC
            IARGCD(2) = KEYX(IDHKSN)
            CALL CDPRNT (LPRTCD, '(/,'' CDPEEK : Error '',I12,'' in '//
     +           'reading data for '',I12)', IARGCD, 2)
          ELSE
            CALL CDPRNT (LPRTCD, '(/,'' CDPEEK : Empty data structure'//
     +           ' for '',I12)', KEYX(IDHKSN), 1)
          ENDIF
        ENDIF
*
        IF (LFRSCX.NE.0) CALL MZDROP (IDIVCD, LFRSCX, 'L')
        IF (IOPHCC.EQ.0) THEN
          READ (LUKYCX, CHSLA(1:NINCR+2), ERR=30, END=30)
        ENDIF
        GO TO 20
      ELSE IF (IOPHCC.NE.0) THEN
        GO TO 20
      ENDIF
*
   30 CALL CDCLFL (LUKYCX)
*
 1001 FORMAT (///)
 1002 FORMAT (/)
 1003 FORMAT (A40)
 1004 FORMAT (I8)
*                                                             END CDPEEK
  999 END
