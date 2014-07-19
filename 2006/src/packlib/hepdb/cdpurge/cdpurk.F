*
* $Id: cdpurk.F,v 1.1.1.1 1996/02/28 16:24:24 mclareni Exp $
*
* $Log: cdpurk.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:24  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDPURK (PATHN, ITIME, MASK, KEYI, CHOPT, IRC)
*     ========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPURK (PATHN, ITIME, MASK, KEYI, CHOPT, IRC*)          *
*                                                                      *
*   Deletes objects in a directory path name steered by a selection    *
*   on a number of key elements                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     ITIME    Time of validity of the object                          *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection. If MASK corresponding to     *
*              one of the fields of 'Beginning' validity range is set, *
*              it will select objects with start validity smaller than *
*              those requested in KEYS. If MASK corresponding to one   *
*              of the fields of 'End' validity range is set, it will   *
*              select objects with end validity larger than those in   *
*              KEYS. If MASK corresponding to time of insertion is set,*
*              objects inserted earlier than KEYS(IDHINS) are selected *
*     KEYI     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     CHOPT    Character string with any of the following characters   *
*          B   Save in the special backup file; not in standard Journal*
*          F   Require a full match of the entire KEYI vector;  MASK   *
*              is ignored                                              *
*          K   Delete object with key serial number as given in KEYI;  *
*              MASK is ignored                                         *
*          S   expect multiple Key banks satisfying selection on a     *
*              number of keys (not compatible with options F or K)     *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user,   CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =111 : Illegal path name                               *
*               =112 : No key for the path name satisfying the Key     *
*                      assignments                                     *
*               =113 : Illegal character option                        *
*               =114 : Valid data objects in the Node/Key structure    *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information on number of data      *
*     objects deleted in the disk                                      *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      DIMENSION       ITIME(9), KEYI(9), MASK(9), KEYS(MXDMCK)
      CHARACTER       CHOPT*(*), PATHN*(*), PATHY*255
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Initialize options
*
      KEY7   = KEY7CK
      KEY7O  = 0
      KEY7CK = 0
      NDEL   = 0
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                         GO TO 999
      IF (IOPMCA.NE.0) THEN
        IRC    = 113
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURK : Illegal'//
     +  ' Character option '')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                         GO TO 999
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      IF (NKEYCK.LE.0) THEN
        IRC    = 112
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURK : No vali'//
     +  'd object for Path Name '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Overwrite MASK option for certain CHOPT
*
      IF (IOPKCA.NE.0) THEN
        CALL VZERO (IOKYCA, NWKYCK)
        IOKYCA(IDHKSN) = 1
        IOPSCA = 0
      ELSE IF (IOPFCA.NE.0) THEN
        DO 5 I = 1, NWKYCK
          IF (I.EQ.IDHKSN.OR.I.EQ.IDHPTR.OR.
     +       (I.GT.NOF1CK.AND.I.LE.(NOF1CK+2*NPARCD))) THEN
            IOKYCA(I) = 0
          ELSE
            IOKYCA(I) = 1
          ENDIF
    5   CONTINUE
        IOPSCA = 0
      ELSE
        IOKYCA(IDHKSN) = 0
      ENDIF
*
* *** Save the command in the journal file
*
      CALL CDKEYT
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
      CALL UCOPY (KEYI, KEYS, NWKYCK)
      KEY7CK = KEY7
      CALL CDSPUR (PATHY, NWKYCK, ITIME, KEYS, 1, IRC)
      KEY7CK = 0
      IF (IRC.NE.0)                         GO TO 999
      IF (IOPPCD.NE.0)                      GO TO 999
#if !defined(CERNLIB__P3CHILD)
*
* *** Prepare the Key banks in memory
*
      CALL CDNODE (PATHY, IRC)
      IF (IRC.NE.0)                         GO TO 999
      IF (LQ(KOFUCD+LBNOCD-KLKYCD).NE.0) THEN
        IRC    = 114
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURK : Valid '//
     +  'KYCD exists for Path Name '//PATHY//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
*
      IF (KEY7.GT.0) THEN
        IF (IOKYCA(IDHINS).NE.0) THEN
          KEYS(IDHINS) = MIN0 (KEYS(IDHINS), KEY7)
        ELSE
          KEYS(IDHINS)   = KEY7
          IOKYCA(IDHINS) = 1
        ENDIF
      ENDIF
      CALL CDKEYB (KEYS, LBDACD, ITIME, IRC)
      IF (IRC.NE.0)                         GO TO 999
      IF (LQ(KOFUCD+LBNOCD-KLKYCD).LE.0) THEN
        IRC    = 112
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURK : No vali'//
     +  'd object for Path Name '//PATHY//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
        GO TO 999
      ENDIF
      LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
      ITEMP  = IOPKCA
      IOPKCA = 1
   10 IF (IOPSCA.EQ.0 .AND. LBKYCD.GT.0) THEN
        DO 15 I = 1, NWKYCK
          IF (IOKYCA(I).NE.0) THEN
            IF (I.EQ.IDHKSN.OR.I.EQ.IDHINS.OR.
     +         (I.GT.NOF1CK.AND.I.LE.(NOF1CK+2*NPARCD))) THEN
              KEYVCK(I) = KEYS(I)
            ELSE
              KEYVCK(I) = IQ(KOFUCD+LBKYCD+I)
            ENDIF
          ELSE
            KEYVCK(I) = 0
          ENDIF
   15   CONTINUE
        CALL CDKXIN (ITIME, IDIVCD, LBDACD, LBKYCD, -1, NWKYCK, KEYVCK,
     +               IPREC, IRC)
        IF (IRC.NE.0)                       GO TO 100
        CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LBKYCD+1), NWKYCK)
        LBKYCD = LQ(KOFUCD+LBKYCD)
        GO TO 10
      ENDIF
      IOPKCA = ITEMP
*
* *** Now mark the objects which are in the Key banks to be deleted
*
      IF (IOPTP.EQ.0) THEN
*
        IPNT   = KOFSCD + LCDRCD + IKDRCD
        ISTP   = NWKYCK + 1
        DO 20 IK = 1, NKEYCK
          KEY1CK(IK) = IQ(IPNT+(IK-1)*ISTP+IDHKSN)
          IPURCK(IK) = 2
   20   CONTINUE
        LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
   25   IF (LBKYCD.GT.0) THEN
          II     = IUCOMP (IQ(KOFUCD+LBKYCD+IDHKSN), KEY1CK, NKEYCK)
          IF (II.GT.0) IPURCK(II) = 0
          LBKYCD = LQ(KOFUCD+LBKYCD)
          GO TO 25
        ENDIF
        CALL CDDELK (IRC)
        NDEL   = IQUEST(2)
*
      ELSE
*
        NKEYS  = NKEYCK
        KST    = NWKYCK + 1
        MAXKY  = -1
        LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
        MINK   = IQ(KOFUCD+LBKYCD+IDHKSN)
        MAXK   = IQ(KOFUCD+LBKYCD+IDHKSN)
   30   LBKYCD = LQ(KOFUCD+LBKYCD)
        IF (LBKYCD.GT.0) THEN
          MINK   = MIN0 (MINK, IQ(KOFUCD+LBKYCD+IDHKSN))
          MAXK   = MAX0 (MAXK, IQ(KOFUCD+LBKYCD+IDHKSN))
          GO TO 30
        ENDIF
*
        DO 55 JK1 = 1, NKEYS
          JK     = NKEYS + 1 - JK1
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          KPNT   = IUHUNT (JK, IQ(IPNT+MPSRCD), NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT   = KPNT + IPNT - MPSRCD
          ELSE
            KPNT   = IPNT + (JK - 1) * KST
          ENDIF
          MINKY  = IQ(KPNT+MOBJCD) + 1
          IF (MAXK.LT.MINKY)                GO TO 50
          IF (MAXKY.GT.0.AND.MINK.GT.MAXKY) GO TO 50
          CALL CDPATH (TOP1CT, JK)
          PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0)  THEN
            IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURK : Ill'//
     +      'egal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 100
          ENDIF
          NKEYCK = IQUEST(7)
          IF (NKEYCK.LE.0)                  GO TO 45
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          ISTP   = NWKYCK + 1
*
          DO 35 IK = 1, NKEYCK
            KEY1CK(IK) = IQ(IPNT+(IK-1)*ISTP+IDHKSN)
            IPURCK(IK) = 2
   35     CONTINUE
          LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
   40     IF (LBKYCD.GT.0) THEN
            II     = IUCOMP (IQ(KOFUCD+LBKYCD+IDHKSN), KEY1CK, NKEYCK)
            IF (II.GT.0) IPURCK(II) = 0
            LBKYCD = LQ(KOFUCD+LBKYCD)
            GO TO 40
          ENDIF
          CALL CDDELK (IRC)
          NDEL   = NDEL + IQUEST(2)
*
   45     CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0)  THEN
            IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURK : Ill'//
     +      'egal Path Name '//PATHY//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
            GO TO 100
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
   50     MAXKY  = MINKY - 1
   55   CONTINUE
*
      ENDIF
*
  100 LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
      IF (LBKYCD.GT.0) CALL MZDROP (IDIVCD, LBKYCD, 'L')
#endif
#if defined(CERNLIB__P3CHILD)
      IRC    = 0
#endif
      IF (IRC.EQ.0) IQUEST(2) = NDEL
*                                                             END CDPURK
  999 END
