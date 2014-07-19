*
* $Id: cdedky.F,v 1.1.1.1 1996/02/28 16:24:49 mclareni Exp $
*
* $Log: cdedky.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:49  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDEDKY (CFNAM, NCF, PATHN, CHOPT, IRC)
*     =================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDEDKY (CFNAM, NCF, PATHN, CHOPT, IRC)                  *
*                                                                      *
*   Edits keys and data in an interactive session                      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     CFNAM    Character string describing the name of the file used   *
*              for editing                                             *
*     NCF      Number of characters in the string CFNAM                *
*     PATHN    Character string describing the pathname                *
*     CHOPT    Character with any of the follwing characters           *
*          A   ASCII type data                                         *
*          S   Standard ZEBRA data bank (default)                      *
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
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/cxlink.inc"
      PARAMETER       (JBIAS=2)
      CHARACTER       CFNAM*(*), PATHN*(*), CHOP1*1, CHOP2*1, CHOP0*4
      CHARACTER       CHSLA*100, STRFL*300, CHOPT*(*), CFMT*20
      DIMENSION       KEYS(MXDMCK), KEYO(MXDMCK), IOPTS(2),ITIME(MXPACD)
      EQUIVALENCE     (IOPTS(1), IOPA), (IOPTS(2), IOPS)
*
*     ------------------------------------------------------------------
*
* *** Open the file for editing
*
      CALL CDOPFL (LUKYCX, CFNAM, 'UNKNOWN', IRC)
      CHOP1   = 'U'
      IF (IRC.NE.0)         GO TO 999
      IF (IOPHCC.NE.0) THEN
        IMAX   = MXDPCC
        IMRK   = 1
        I1     = 2
        I2     = 9
        CFMT   = CFMTCC
      ELSE
        IMAX   = 80
        IMRK   = 40
        I1     = 23
        I2     = 30
        CFMT   = '(A80)'
      ENDIF
      CALL CDDISP (LUKYCX, PATHN, CHOP1, IRC)
      CALL CDCLFL (LUKYCX)
      CALL UOPTC (CHOPT, 'AS', IOPTS)
      IF (IRC.NE.0)         GO TO 999
      CALL KUEDIT (CFNAM, IST)
*
* *** Read back the edited file
*
      CALL CDOPFL (LUKYCX, CFNAM, 'OLD', ISTAT)
      IF (ISTAT.NE.0)       GO TO 999
      CHOP2  = ' '
      CALL VZERO (KEYO, MXDMCK)
      CHSLA  = ' '
      NINCR  = NWKYCK - NSKPCK - 1
      IF (IOPHCC.NE.0) THEN
        READ (LUKYCX, 1001, ERR=20, END=20)
      ELSE
        READ (LUKYCX, 1002, ERR=20, END=20)
        CHSLA(1:1) = '('
        CHSLA(NINCR+2:NINCR+2) = ')'
        DO 5 I = 1, NINCR
    5   CHSLA(I+1:I+1) = '/'
      ENDIF
      DO 10 I = 1, NPARCD
        ITIME(I) = 1
   10 CONTINUE
*
   15 READ (LUKYCX, CFMT, ERR=20, END=20) STRFL(1:IMAX)
      IF ((IOPHCC.NE.0) .AND. (STRFL(1:20).EQ.' ')) THEN
        GO TO 20
      ELSE IF ((IOPHCC.EQ.0) .AND. (STRFL(IMRK:IMRK).EQ.'D')) THEN
        READ (LUKYCX, CHSLA(1:NINCR+2), ERR=20, END=20)
        GO TO 15
      ELSE IF (STRFL(IMRK:IMRK).EQ.'N') THEN
*
*  **   The object is to be inserted
*
        CALL VZERO (KEYS, NWKYCK)
        IF (IOPHCC.NE.0) THEN
          CALL CDDCKH (STRFL, KEYS(1), IKEY, IRC)
          KEYS(IDHKSN) = IKEY
          CHOP0   = 'V'
        ELSE
          READ (STRFL(21:30), 1004, ERR=20) KEYS(IDHKSN)
          CALL CDDCKV (LUKYCX, ICONCK(1), KEYS(1), IRC)
          CHOP0   = 'H'
        ENDIF
        IF (IRC.NE.0)       GO TO 20
        IF (CHOP1.EQ.'I') THEN
          CHOP0  = CHOP0(1:1)//'EI'
        ELSE IF (CHOP2.EQ.'X') THEN
          CHOP0  = CHOP0(1:1)//CHOP2
        ELSE
          CHOP0  = CHOP0(1:1)//'I'
        ENDIF
        IOPX   = IOPXCA
        IF (IOPA.EQ.0) THEN
          CALL CDUPKY (KEYS, KEYO, PATHN, CHOP0, IRC)
        ELSE
          CALL CDAIWR (KEYS, KEYO, PATHN, CHOP0, IRC)
        ENDIF
        IOPXCA = IOPX
        IF (IRC.NE.0)       GO TO 20
        GO TO 15
      ELSE IF (STRFL(IMRK:IMRK).EQ.'R') THEN
*
*  **   An existing object is modified
*
        READ (STRFL(I1:I2), 1005, ERR=20) KEYO(IDHKSN)
        IOLD1  = IOKYCA(IDHKSN)
        IOLDK  = IOPKCA
        IOPKCA = 1
        IOKYCA(IDHKSN) = 1
        CALL CDKXIN (ITIME, IDISCD, LAUXCL(7), LAUXCL(7), JBIAS, NWKEY,
     +               KEYO, IPREC, IRC)
        IOPKCA = IOLDK
        IOKYCA(IDHKSN) = IOLD1
        CHOP2  = 'X'
        IF (IOPHCC.EQ.0) READ (LUKYCX, CHSLA(1:NINCR+2), ERR=20, END=20)
        GO TO 15
      ELSE IF (IOPHCC.NE.0) THEN
        GO TO 15
      ENDIF
*
   20 CALL CDCLFL (LUKYCX)
*
 1001 FORMAT (///)
 1002 FORMAT (/)
 1003 FORMAT (A)
 1004 FORMAT (I10)
 1005 FORMAT (I8)
*                                                             END CDEDKY
  999 END
