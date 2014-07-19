*
* $Id: cdrzin.F,v 1.1.1.1 1996/02/28 16:24:31 mclareni Exp $
*
* $Log: cdrzin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDRZIN (IDIV, LAD, JBIAS, ICUR, ICYCL, PATHN, IRC)
*     =============================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRZIN (IDIV, *LAD*, JBIAS, ICUR, ICYCL*, PATHN, IRC*)  *
*                                                                      *
*   Checks if enough space is left in memory before loading the data   *
*   part of an object. When not enough space is available after        *
*   garbage collection, the banks freed by DBFREE are dropped.         *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDIV     Division number where the object is to be created       *
*     LAD      Address of the data bank as returned by RZIN            *
*     JBIAS    Link bias as supplied to RZIN                           *
*     ICUR     RZ key number of the object to be loaded                *
*     ICYCL    Cycle number of the data object                         *
*     PATHN    Path name (only needed in the printout)                 *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCDIC, CDCOMP, CDDDIR, CDKXIN, CDPART, CDPURP, CDRTFZ,  *
*             CDUDIC, CDUNCP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 34 : Error in RZ for reading the object              *
*               = 99 : No space in memory for creating the bank        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
      CHARACTER       RZDIP3*80
#endif
      DIMENSION       LAD(9)
      CHARACTER       PATHD*80, PATHN*(*)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*     ------------------------------------------------------------------
#if defined(CERNLIB__P3CHILD)
*
      IF (IPASP3.EQ.1) THEN
*
* *** Stack the directory names and the key sequence numbers
*
        MSDBP3 = ' '
        CALL RZCDIR (MSDBP3, 'R...')
        IF (NDIRP3.EQ.0.OR.MSDBP3.NE.RZDIP3) THEN
*            New directory
          NDIRP3 = NDIRP3+1
          RZDIP3 = MSDBP3
          IF (NDIRP3.EQ.1) THEN
            INDXP3 = 1
          ELSE
            INDXP3 = INDXP3+IQ(KOFUCD+LNK1P3+INDXP3+1)+21
          ENDIF
          IF (INDXP3+22.GT.IQ(KOFUCD+LNK1P3-1))
     +        CALL MZPUSH (IXDBP3, LNK1P3, 0, 122, '....')
          CALL UCTOH  (RZDIP3, IWDBP3, 4, 80)
          CALL ZHTOI  (IWDBP3, IQ(KOFUCD+LNK1P3+INDXP3+2), 20)
          NBKDP3 = 0
        ENDIF
*         New key in this directory
        NBKDP3 = NBKDP3+1
        IQ(KOFUCD+LNK1P3+INDXP3+1) = NBKDP3
        IND    = INDXP3+NBKDP3+21
        IF (IND.GT.IQ(KOFUCD+LNK1P3-1)-20)
     +    CALL MZPUSH (IDISCD, LNK1P3, 0, 100, '....')
        IQ(KOFUCD+LNK1P3+IND) = ICUR
*
        NBKYP3 = NBKYP3+1
        IRC    = 0
        GO TO 999
*
      ELSE IF (IPASP3.EQ.2) THEN
        IF (LNK4P3.EQ.0.OR.LNK5P3.EQ.0) THEN
          IRC    = 34
          GO TO 990
        ENDIF
*         Get size of data object
        NWDS   = IQ(KOFUCD+LNK4P3-1)
        GO TO 5
*
      ENDIF
#endif
*
* *** Find number of data words
*
      IPNT   = KOFSCD + LCDRCD + IKDRCD + (ICUR - 1) * (NWKYCK + 1)
      LCYC   = IQ(IPNT)
      NWDS   = JBYT (IQ(KOFSCD+LCDRCD+LCYC+3), 1, 20)
      IRC    = 0
*
* *** See if enough space is available in memory
*
    5 NEEDW  = NWDS + 100
      CALL MZNEED (IDIV, NEEDW, ' ')
      IF (IQUEST(11).LT.0)  THEN
        CALL MZNEED (IDIV, NEEDW, 'G')
        IF (IQUEST(11).LT.0)  THEN
          LGO    = 0
   10     LBFYCD = LZFIDH (IDIV, IHKYCD, LGO)
          IF (LBFYCD.NE.0)  THEN
            ND0    = IQ(KOFUCD+LBFYCD-1)
            IF (IQ(KOFUCD+LBFYCD+ND0+MKYFRI).GT.0)  THEN
              LDAT   = LQ(KOFUCD+LBFYCD-KLDACD)
              IF (LDAT.GT.0) CALL MZDROP (IDIV, LDAT, 'L')
            ENDIF
            LGO    = LBFYCD
            GO TO 10
          ENDIF
          CALL MZNEED (IDIV, NEEDW, 'G')
          IF (IQUEST(11).LT.0)  THEN
            IRC    = 99
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              CALL CDPRNT (LPRTCD, '(/,'' CDRZIN : No space left for '//
     +             'loading object from disk == space needed '',I12)',
     +             IQUEST(11), 1)
            ENDIF
#endif
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
*
* *** Now call RZIN
*
#if defined(CERNLIB__P3CHILD)
      IF (IPASP3.EQ.2) THEN
        CALL MZCOPY (IXDBP3, LNK4P3, IDIV, LAD(1), JBIAS, 'P...')
        CALL UCOPY  (IQ(KOFUCD+LNK5P3+1), IQUEST(96), 5)
        LNK4P3 = LQ(KOFUCD+LNK4P3)
        LNK5P3 = LQ(KOFUCD+LNK5P3)
        IF (IRC.NE.0) GO TO 999
        IF (JBIAS.GT.0) THEN
          LBANK = LAD(1)
        ELSE
          LBANK = LQ(KOFUCD+LAD(1)+JBIAS)
        ENDIF
        IQUEST(11) = LBANK
        GO TO 999
      ENDIF
*
#endif
      ICYCL  = 999999
      CALL RZIN (IDIV, LAD(1), JBIAS, ICUR, ICYCL, 'S')
  990 IF (IQUEST(1).NE.0) THEN
        IRC    = 34
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          PATHD  = PATHN
          NCH    = LENOCC (PATHD)
          CALL CDPRNT (LPRTCD, '(/,'' CDRZIN : RZIN error for path '//
     +         'name '//PATHD(1:NCH)//''')', IARGCD, 0)
        ENDIF
#endif
      ENDIF
*                                                             END CDRZIN
  999 END
