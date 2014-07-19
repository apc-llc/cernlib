*
* $Id: cdgetdb.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdgetdb.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDGETDB (PATHN, LBK, MASK,KEYS, KYSR,KYINM, CHOPT, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDGETDB (PATHN, LBK*, MASK,KEYS, KYSR,KYINM, CHOPT,IRC*)*
*                                                                      *
*   Prepares the database data structure in memory for any required    *
*   Pathname and set of Keys, unless already done.  Returns the        *
*   in memory for the corresponding Key bank(s) with a selection on    *
*   a range of start validity time and user keys.                      *
*   Selects objects with validity range as specified in KEYS(NOF1CK..) *
*   if the proper masks are used. It sees the object in data base has  *
*   the start validity period within the range specified by the user.  *
*   Selection on insertion time demands data base object was inserted  *
*   before the selected time.                                          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     LBK      Address(es) of Keys bank(s) KYCD. The data bank address *
*              can be obtained as LQ(LBK-1)                            *
*              For option 'S' it is the support address of the linear  *
*              structure                                               *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection. If MASK corresponding to     *
*              one of the fields of 'Beginning' validity range is set, *
*              it will select objects with start validity larger than  *
*              those requested in KEYS. If MASK corresponding to one   *
*              of the fields of 'End' validity range is set, it will   *
*              select objects with start validity smaller than those   *
*              in the KEYS vector (in the fields corresponding to end  *
*              validity). If MASK corresponding to time of insertion   *
*              is set, objects inserted earlier than KEYS(IDHINS) are  *
*              selected                                                *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     KYSR     The limits on the serial number for option R            *
*     KYINM    The lower limit on insertion time used with option I    *
*     CHOPT    Character string with any of the following characters   *
*          I   ignore objects inserted before KYINM                    *
*          K   read only the keys (no data is required)                *
*          R   select on range of serial number given in KYSR(1:2)     *
*          S   expect multiple Key banks satisfying selection on a     *
*              number of keys                                          *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  2 : Illegal path name                               *
*               = 32 : No keys/data in this directory                  *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information on number of data      *
*     objects selected by CDGETDB                                      *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (NZ=0)
      DIMENSION       KEYS(9), MASK(9), KYSR(2), LBK(9)
      CHARACTER       CHOPT*(*), PATHN*(*), PATHY*255
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Initialize options
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                                      GO TO 999
      CALL UCOPY (MASK, IOKYCA, MXDMCK)
      IOPMCA = 0
      LBK(1) = 0
*
* *** Create (or complete) database skeleton in memory
*
      CALL CDNODE (PATHN, IRC)
      IF (IRC.NE.0)                                      GO TO 999
      PATHY  = PAT1CT
*
*  ** Start from the end of the existing chain
*
      IF (LQ(KOFUCD+LBNOCD-KLKYCD).NE.0) THEN
        LFIXCD = LZLAST (IDIVCD, LQ(KOFUCD+LBNOCD-KLKYCD))
        IF (LFIXCD.EQ.0) THEN
          LFIXCD = LBNOCD
          JBIAS  =-KLKYCD
        ELSE
          JBIAS  = 0
        ENDIF
      ELSE
        LFIXCD = LBNOCD
        JBIAS  =-KLKYCD
      ENDIF
      NDK    = IQ(KOFUCD+LBNOCD+MNDNWD)
      CALL UCOPY (IQ(KOFUCD+LBNOCD+MNDIOF), IOKYCD, NWNOCD)
*
* *** Set the current directory
*
      CALL RZCDIR (PATHY, ' ')
      IF (IQUEST(1).NE.0) THEN
        IRC    = 2
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGETDB : Illega'//
     +  'l path name '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      CALL CDKEYT
*
      IF (NKEYCK.LE.0) THEN
        IRC    = 32
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGETDB : No key'//
     +  's/data for  '//PATHY//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      NCHR   = LENOCC (PATHY)
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
* *** Now create the key bank(s) and optionally the data bank
*
      NKB    = 0
      IPRBCA = ISIGN (IPRBCA, -1)
      IPRECA = ISIGN (IPRECA, -1)
      IF (IPRBCA.EQ.0.AND.IPRECA.EQ.0) THEN
        IFLG   = 99
      ELSE
        IFLG   = 0
      ENDIF
#if defined(CERNLIB__P3CHILD)
      LCOND = (IOPSCA.NE.0.AND.IOPKCA.EQ.0)
      CALL CDSTP3 (1, LCOND, NBKP3, 0)
#endif
      IF (IOPTP.EQ.0) THEN
        DO 20 JK = 1, NKEYCK
          IK     = NKEYCK + 1 - JK
          CALL CDKEYR (IK, NWKYCK, KEYVCK)
          CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
          IF (ISEL.NE.0)                                 GO TO 20
          IF ((KEYVCK(IDHKSN).LT.KYSR(1).OR.KEYVCK(IDHKSN).GT.KYSR(2))
     +        .AND.(IOPRCA.NE.0))                        GO TO 20
          IF (IOPICA.NE.0.AND.KEYVCK(IDHINS).LT.KYINM)   GO TO 20
          IF (JBIT(KEYVCK(IDHFLG),JIGNCD).NE.0)          GO TO 20
          CALL CDBANK (IDIVCD, LBKYCD, LFIXCD, JBIAS, 'KYCD', NLKYCD,
     +                 NSKYCD, NDK, IOKYCD, NZ, IRC)
          IF (IRC.NE.0)                                  GO TO 999
          LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
          LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
          IQ(KOFUCD+LBKYCD+NDK+MKYFRI) = 0
          IQ(KOFUCD+LBKYCD+NDK+MKYCRU) = IQ(KOFUCD+LBKYCD+NDK+MKYCRU) +1
          IQ(KOFUCD+LBKYCD+NDK+MKYCEV) = IQ(KOFUCD+LBKYCD+NDK+MKYCEV) +1
          NKB    = NKB + 1
          IF (NKB.EQ.1) LBK(1) = LBKYCD
          LFIXCD = LBKYCD
          JBIAS  = 0
          IF (IOPKCA.EQ.0) THEN
#if defined(CERNLIB__P3CHILD)
            IF (IPASP3.EQ.1) CALL CDSTP3 (2, LCOND, NBKYP3, 0)
#endif
            CALL VZERO (KEYVCK, NWKYCK)
            KEYVCK(IDHKSN) = IK
            IOKYCA(IDHKSN) = 1
            CALL CDKXIN (ITIME, IDIVCD, LAUXCL(9), LBKYCD, -KLDACD,
     +                   NWKEY, KEYVCK, IPREC, IRC)
            IOKYCA(IDHKSN) = 0
            LAUXCL(9) = 0
            IQ(KOFUCD+LBKYCD+NDK+MKYPRE) = IPREC
            IQ(KOFUCD+LBKYCD+NDK+MKYRID) = IQ(KOFUCD+LBKYCD+NDK+MKYRID)
     +                                   + 1
          ENDIF
          CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LBKYCD+1), NWKYCK)
          DO 10 I = 1, NPARCD-1
            IQ(KOFUCD+LBKYCD+NWKYCK+I) = IQ(KOFUCD+LBKYCD+NOF1CK+2*I-1)
   10     CONTINUE
          IQ(KOFUCD+LBKYCD+NWKYCK+NPARCD) =
     +      IQ(KOFUCD+LBKYCD+NOF1CK+2*NPARCD-1) + 1
          IF (IRC.NE.0)                                  GO TO 999
          IF (IOPSCA.EQ.0) THEN
            IQUEST(2) = NKB
            GO TO 999
          ENDIF
   20   CONTINUE
*
      ELSE
        KST    = NWKYCK + 1
        NKEYS  = NKEYCK
        KSERL  = -1
        KINSL  = 0
        DO 40 JKK = 1, NKEYS
          IKK    = NKEYS + 1 - JKK
          KPNT   = IUHUNT (IKK, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                     NKEYS*KST, KST)
          IF (KPNT.GT.0) THEN
            KPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
          ELSE
            KPNT   = KOFSCD + LCDRCD + IKDRCD + (IKK - 1) * KST
          ENDIF
          IF (IOPICA.NE.0) THEN
            IF (JKK.GT.1.AND.KINSL.LT.KYINM)             GO TO 40
            KINSL  = IQ(KPNT+IDHINS)
          ENDIF
          IF (IOPRCA.NE.0) THEN
            IF (IQ(KPNT+MOBJCD).GT.KYSR(2))              GO TO 40
            IF (KSERL.GE.0.AND.KSERL.LT.KYSR(1))         GO TO 40
            KSERL  = IQ(KPNT+MOBJCD)
          ENDIF
          CALL CDPSEL (ITIME, KEYS, IQ(KPNT+1), IFLG, ISEL)
          IF (ISEL.NE.0)                                 GO TO 40
*
          CALL CDPATH (TOP2CT, IKK)
          PAT2CT = PATHY(1:NCHR)//'/'//TOP2CT
          CALL RZCDIR (PAT2CT, ' ')
          IF (IQUEST(1).NE.0) THEN
            IQUEST(1) = 2
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGETDB : Il'//
     +      'legal path name '//PAT2CT//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NKEYCK = IQUEST(7)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
#if defined(CERNLIB__P3CHILD)
          NKBP3  = 0
#endif
          DO 30 JK = 1, NKEYCK
            IK     = NKEYCK + 1 - JK
            CALL CDKEYR (IK, NWKYCK, KEYVCK)
            CALL CDKSEL (ITIME, KEYS, KEYVCK, IFLG, ISEL, INBR)
            IF (ISEL.NE.0)                               GO TO 30
            IF ((KEYVCK(IDHKSN).LT.KYSR(1).OR.KEYVCK(IDHKSN).GT.KYSR(2))
     +          .AND.(IOPRCA.NE.0))                      GO TO 30
            IF (IOPICA.NE.0.AND.KEYVCK(IDHINS).LT.KYINM) GO TO 30
            IF (JBIT(KEYVCK(IDHFLG),JIGNCD).NE.0)        GO TO 30
            CALL CDBANK (IDIVCD, LBKYCD, LFIXCD, JBIAS, 'KYCD', NLKYCD,
     +                   NSKYCD, NDK, IOKYCD, NZ, IRC)
            IF (IRC.NE.0)                                GO TO 999
            LQ(KOFUCD+LBKYCD-KLNOCD) = LBNOCD
            LQ(KOFUCD+LBKYCD-KLUPCD) = LBUPCD
            IQ(KOFUCD+LBKYCD+NDK+MKYFRI) = 0
            IQ(KOFUCD+LBKYCD+NDK+MKYCRU) = IQ(KOFUCD+LBKYCD+NDK+MKYCRU)
     +                                   + 1
            IQ(KOFUCD+LBKYCD+NDK+MKYCEV) = IQ(KOFUCD+LBKYCD+NDK+MKYCEV)
     +                                   + 1
            NKB    = NKB + 1
            IF (NKB.EQ.1) LBK(1) = LBKYCD
            LFIXCD = LBKYCD
            JBIAS  = 0
            IF (IOPKCA.EQ.0) THEN
#if defined(CERNLIB__P3CHILD)
              IF (IPASP3.EQ.1) CALL CDSTP3 (2, LCOND, NBKYP3, 0)
#endif
              CALL VZERO (KEYVCK, NWKYCK)
              KEYVCK(IDHKSN) = IK
              IOKYCA(IDHKSN) = 1
              CALL CDKXIN (ITIME, IDIVCD, LAUXCL(9), LBKYCD, -KLDACD,
     +                     NWKEY, KEYVCK, IPREC, IRC)
              IOKYCA(IDHKSN) = 0
              LAUXCL(9) = 0
              IQ(KOFUCD+LBKYCD+NDK+MKYPRE) = IPREC
              IQ(KOFUCD+LBKYCD+NDK+MKYRID) =IQ(KOFUCD+LBKYCD+NDK+MKYRID)
     +                                     + 1
            ENDIF
            CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LBKYCD+1), NWKYCK)
            DO 25 I = 1, NPARCD-1
              IQ(KOFUCD+LBKYCD+NWKYCK+I) =IQ(KOFUCD+LBKYCD+NOF1CK+2*I-1)
   25       CONTINUE
            IQ(KOFUCD+LBKYCD+NWKYCK+NPARCD) =
     +        IQ(KOFUCD+LBKYCD+NOF1CK+2*NPARCD-1) + 1
            IOKYCA(IDHKSN) = 0
            IF (IRC.NE.0)                                GO TO 999
            IF (IOPSCA.EQ.0) THEN
              IQUEST(2) = NKB
              GO TO 999
            ENDIF
   30     CONTINUE
          CALL RZCDIR (PATHY, ' ')
          IF (IQUEST(1).NE.0) THEN
            IRC    = 2
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDGETDB : Il'//
     +      'legal path name '//PATHY//''')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
   40   CONTINUE
*
      ENDIF
*
  998 IQUEST(2) = NKB
#if defined(CERNLIB__P3CHILD)
*
      CALL CDUSP3 ('CDGETDB', ITIME, IRC)
#endif
*
  999 CONTINUE
#if defined(CERNLIB__P3CHILD)
      IF (LNK3P3.NE.0) CALL MZDROP (IXDBP3, LNK3P3, '....')
      LNK3P3 = 0
      LNK4P3 = 0
      LNK5P3 = 0
      NBKYP3 = 0
      NDIRP3 = 0
      IPASP3 = 0
#endif
*                                                            END CDGETDB
      END
