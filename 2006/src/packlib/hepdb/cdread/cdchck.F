*
* $Id: cdchck.F,v 1.1.1.1 1996/02/28 16:24:25 mclareni Exp $
*
* $Log: cdchck.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:25  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDCHCK (LBK, ITIME, KEYS, LBD, IRC)
*     ==============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCHCK (LBK, ITIME, KEYS, *LBD*, IRC*)                  *
*                                                                      *
*   Loads data from disk according to validity time and Key values     *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LBK      Address of the bank containing the keys                 *
*     ITIME    Time for which the valid data are required              *
*     KEYS     Vector of keys                                          *
*     LBD      Address of the bank containing the data                 *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDUSE,  CDUSEDB,CDUSEM                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 31 : Path name in Node bank is wrong                 *
*               = 32 : No keys/data in this directory                  *
*               = 36 : Data bank address zero on return from CDKXIN    *
*               = 37 : Insufficient space in USER store array          *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ctkxin.inc"
#include "hepdb/ctpath.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (JBIAS=2)
      DIMENSION       LBK(9), LBD(9), KEYS(9), ITIME(9)
      CHARACTER       PATHN*255
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Lock the bank
*
      LREFCD(2) = LBK(1)
      IQ2    = IQUEST(2)
      NDK    = IQ(KOFUCD+LREFCD(2)-1)
      IQ(KOFUCD+LREFCD(2)+NDK+MKYFRI) = 0
      IQ(KOFUCD+LREFCD(2)+NDK+MKYCRU) = IQ(KOFUCD+LREFCD(2)+NDK+MKYCRU)
     +                                + 1
      IQ(KOFUCD+LREFCD(2)+NDK+MKYCEV) = IQ(KOFUCD+LREFCD(2)+NDK+MKYCEV)
     +                                + 1
*
* *** Get the pathname
*
      LBNOCD = LQ(KOFUCD+LREFCD(2)-KLNOCD)
      NCHAR  = IQ(KOFUCD+LBNOCD+MNDNCH)
      CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT4CT, NCHAR)
      PATHN  = PAT4CT(1:NCHAR)
*
* *** Set the current directory
*
      CALL RZCDIR (PATHN, ' ')
      IF (IQUEST(1).NE.0)  THEN
        IRC    = 31
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK : '//
     +  'Path name '//PATHN//' from node bank illegal'')', IQ, 0)
#endif
        GO TO 998
      ENDIF
      NKEYCK = IQUEST(7)
      NWKYCK = IQUEST(8)
      LCDRCD = IQUEST(11)
      IKDRCD = IQUEST(13)
      CALL CDKEYT
*
* *** Check the number of keys
*
      IF (NKEYCK.EQ.0)  THEN
        IRC    = 32
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK : No key '//
     +  'or data for Path Name '//PATHN//''')', IARGCD, 0)
#endif
        GO TO 998
      ENDIF
*
      IRC    = 0
      IF (IOPKCA.NE.0 .AND. IOPSCA.NE.0) THEN
        GO TO 998
      ENDIF
*
* *** Force retrieval
*
      IF (IOPFCA.NE.0) THEN
        LBD(1) = LQ(KOFUCD+LREFCD(2)-KLDACD)
        IF (LBD(1).NE.0) THEN
          CALL MZDROP (IDIVCD, LBD(1), 'L')
          LBD(1) = 0
        ENDIF
      ENDIF
*
      IF (LQ(KOFUCD+LREFCD(2)-KLDACD).EQ.0) THEN
*
*  **   The bank does not exist yet - retrieve data from the Data Base
*  **   Check which keys are to be used
*
        IF (IOPSCA.EQ.0)  THEN
          DO 10 I = 1, NWKYCK
            IF (IOKYCA(I).NE.0)  THEN
              IF (I.EQ.IDHINS.OR.(I.GE.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD))
     +                                                              THEN
                KEYVCK(I) = KEYS(I)
              ELSE
                KEYVCK(I) = IQ(KOFUCD+LREFCD(2)+I)
              ENDIF
            ELSE
              KEYVCK(I) = 0
            ENDIF
   10     CONTINUE
        ELSE
          CALL UCOPY (IQ(KOFUCD+LREFCD(2)+1), KEYVCK, NWKYCK)
        ENDIF
*
*  **   Retrieve data
*
        CALL CDKXIN (ITIME, IDIVCD, LBD(1), LBK(1), -KLDACD, NWKEY,
     +               KEYVCK, IPREC, IRC)
        IF (IRC.NE.0)                                          GO TO 998
        LREFCD(2) = LBK(1)
        LREFCD(3) = LBD(1)
        IF (IOPSCA.EQ.0) THEN
          IF (IQ(KOFUCD+LREFCD(2)+IDHKSN).NE.KEYVCK(IDHKSN)) IQ2 = 1
        ELSE
          IQ2 = 1
        ENDIF
*
*  *    Overwrite keys in key-bank
*
        CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LREFCD(2)+1), NWKYCK)
        IQ(KOFUCD+LREFCD(2)+NDK+MKYPRE) = IPREC
        IQ(KOFUCD+LREFCD(2)+NDK+MKYRID) =IQ(KOFUCD+LREFCD(2)+NDK+MKYRID)
     +                                  + 1
        IF (IOPSCA.EQ.0) THEN
          IF (IPRBCA+IPRECA.EQ.0) THEN
            IF (IHFLCD.EQ.0) THEN
              KYEN   = 0
              DO 20 I = 1, NPARCD
                IF (KYEN.EQ.0) THEN
                  IF (IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I).LT.KYENCD(I))
     +                                                        KYEN = -1
                  IF (IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I).GT.KYENCD(I))
     +                                                        KYEN =  1
                ENDIF
   20         CONTINUE
              DO I = 1, NPARCD
                IF (KYEN.LT.0) THEN
                  IQ(KOFUCD+LREFCD(2)+NWKYCK+I) =
     +                      IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I)
                ELSE
                  IQ(KOFUCD+LREFCD(2)+NWKYCK+I) = KYENCD(I)
                ENDIF
              ENDDO
            ELSE
              DO 25 I = 1, NPARCD
                IQ(KOFUCD+LREFCD(2)+NWKYCK+I) =
     +          MIN (IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I), KYENCD(I))
   25         CONTINUE
            ENDIF
          ELSE
            DO 30 I = 1, NPARCD-1
              IQ(KOFUCD+LREFCD(2)+NWKYCK+I) =
     +          IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1)
   30       CONTINUE
            IQ(KOFUCD+LREFCD(2)+NWKYCK+NPARCD) =
     +        IQ(KOFUCD+LREFCD(2)+NOF1CK+2*NPARCD-1) + 1
          ENDIF
        ENDIF
*
      ELSE
*
*  **   The data bank exists already
*
        LBD(1) = LQ(KOFUCD+LREFCD(2)-KLDACD)
        LREFCD(3) = LBD(1)
*
*  **   Check if data are valid
*
        KEY6   = IQ(KOFUCD+LREFCD(2)+IDHFLG)
        IF (IOPACA.NE.0) THEN
          IBEG   = 0
          IEND   = 0
          DO 35 I = 1, NPARCD
            IF (IHFLCD.EQ.0) THEN
              IF (IBEG.EQ.0) THEN
                IF (ITIME(I).LT.IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1))
     +                                                         IBEG =-I
                IF (ITIME(I).GT.IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1))
     +                                                         IBEG = I
              ENDIF
              IF (IEND.EQ.0) THEN
                IF (ITIME(I).GT.IQ(KOFUCD+LREFCD(2)+NWKYCK+I)) IEND =-I
                IF (ITIME(I).LT.IQ(KOFUCD+LREFCD(2)+NWKYCK+I)) IEND = I
            ENDIF
            ELSE
              IF (IBEG.EQ.0) THEN
                IF (ITIME(I).LT.IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1))
     +                                                         IBEG =-I
              ENDIF
              IF (IEND.EQ.0) THEN
                IF (ITIME(I).GT.IQ(KOFUCD+LREFCD(2)+NWKYCK+I)) IEND =-I
              ENDIF
            ENDIF
   35     CONTINUE
          IF (IBEG.GE.0.AND.IEND.GE.0)                         GO TO 998
*
        ELSE
*
          IBEG   = 0
          IEND   = 0
          DO 40 I = 1, NPARCD
            IF (IHFLCD.EQ.0) THEN
              IF (IBEG.EQ.0) THEN
                IF (ITIME(I).LT.IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1))
     +                                                         IBEG =-I
                IF (ITIME(I).GT.IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1))
     +                                                         IBEG = I
              ENDIF
              IF (IEND.EQ.0) THEN
                IF (ITIME(I).GT.IQ(KOFUCD+LREFCD(2)+NWKYCK+I)) IEND =-I
                IF (ITIME(I).LT.IQ(KOFUCD+LREFCD(2)+NWKYCK+I)) IEND = I
            ENDIF
            ELSE
              IF (IBEG.EQ.0) THEN
                IF (ITIME(I).LT.IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1))
     +                                                         IBEG =-I
              ENDIF
              IF (IEND.EQ.0) THEN
                IF (ITIME(I).GT.IQ(KOFUCD+LREFCD(2)+NWKYCK+I)) IEND =-I
              ENDIF
            ENDIF
   40     CONTINUE
          IF (IBEG.LT.0.OR.IEND.LT.0)                          GO TO 85
          ISTP   = NWKYCK + 1
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          MNKYCT = -998
          KEY1   = IQ(KOFUCD+LREFCD(2)+IDHKSN)
          KEY6CT = 0
          IMINCT = 1
          IOPTP  = JBIT (IQ(IPNT+IDHFLG), JPRTCD)
*
          IF (IOPTP.EQ.0)  THEN
*
*  *        For non-partitioned data
*
            IMIN   = IUHUNT (KEY1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
            IF (IMIN.GT.0) THEN
              IMINCT = (IMIN - IDHKSN) / ISTP + 1
            ELSE
              IMINCT = 1
            ENDIF
            CALL CDKEYR (IMINCT, NWKYCK, KEYVCK)
            DO 45 I = 1, NWKYCK
              IF (KEYVCK(I).NE.IQ(KOFUCD+LREFCD(2)+I))         GO TO 85
   45       CONTINUE
   50       IF (IMINCT.EQ.NKEYCK)                              GO TO 998
            ICURCT = 1
            CALL CDCHKY (ITIME, KEYS)
            IF (KEY1.EQ.MNKYCT.AND.JBIT(KEY6CT,JIGNCD).EQ.0)   GO TO 998
*
          ELSE
*
*  *        For partitioned data
*
            NKEYS  = NKEYCK
            KST    = NWKYCK + 1
            ICUR   = NKEYS
            DO 55 JK = 1, NKEYS
              KPNT   = IUHUNT (JK, IQ(IPNT+MPSRCD), NKEYS*KST, KST)
              IF (KPNT.GT.0) THEN
                KPNT   = KPNT + IPNT - MPSRCD
              ELSE
                KPNT   = IPNT + (JK - 1) * KST
              ENDIF
              IF (IQ(KPNT+MOBJCD).GT.KEY1) THEN
                ICUR   = IQ(KPNT+MPSRCD) - 1
                GO TO 60
              ENDIF
   55       CONTINUE
*
   60       CALL CDPATH (TOP2CT, ICUR)
            PAT2CT = PATHN(1:NCHAR)//'/'//TOP2CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 31
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK : '//
     +        'Path name '//PAT2CT//' from node bank illegal'')', IQ, 0)
#endif
              GO TO 998
            ENDIF
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            IPNT   = KOFSCD + LCDRCD + IKDRCD
            ISTP   = NWKYCK + 1
*
            IF (NKEYCK.GT.0) THEN
              IMIN   = IUHUNT (KEY1, IQ(IPNT+IDHKSN), NKEYCK*ISTP, ISTP)
              IF (IMIN.GT.0)  THEN
                IMINCT = (IMIN - IDHKSN) / ISTP + 1
              ELSE
                IMINCT = 1
              ENDIF
            ENDIF
            CALL CDKEYR (IMINCT, NWKYCK, KEYVCK)
            DO 65 I = 1, NWKYCK
              IF (KEYVCK(I).NE.IQ(KOFUCD+LREFCD(2)+I))         GO TO 85
   65       CONTINUE
*
   70       IF (ICUR.EQ.NKEYS.AND.IMINCT.EQ.NKEYCK)            GO TO 998
            IMIN   = IMINCT
            MNKYCT = KEY1
            IUSECT = ICUR
*
            DO 75 JK = ICUR, NKEYS
              ICURCT = NKEYS + ICUR - JK
*
              CALL RZCDIR (PATHN, ' ')
              IF (IQUEST(1).NE.0)  THEN
                IRC    = 31
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK :'//
     +          ' Path name '//PATHN//' from node bank illegal'')',IQ,0)
#endif
                GO TO 998
              ENDIF
              NKEYCK = IQUEST(7)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              KPNT   = IUHUNT (ICURCT, IQ(KOFSCD+LCDRCD+IKDRCD+MPSRCD),
     +                         NKEYS*KST, KST)
              IF (KPNT.NE.0) THEN
                IPNT   = KOFSCD + LCDRCD + IKDRCD + KPNT - MPSRCD
              ELSE
                IPNT   = KOFSCD + LCDRCD + IKDRCD + (ICURCT - 1) * KST
              ENDIF
              CALL CDPSEL (ITIME, KEYS, IQ(IPNT+1), 0, ISEL)
              IF (ISEL.NE.0)                                   GO TO 75
*
              CALL CDPATH (TOP2CT, ICURCT)
              PAT2CT = PATHN(1:NCHAR)//'/'//TOP2CT
              CALL RZCDIR (PAT2CT, ' ')
              IF (IQUEST(1).NE.0)  THEN
                IRC    = 31
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK :'//
     +          ' Path name '//PAT2CT//' from node bank illegal'')',IQ
     +          ,0)
#endif
                GO TO 998
              ENDIF
              NKEYCK = IQUEST(7)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              IF (ICURCT.EQ.ICUR) THEN
                IMINCT = IMIN
              ELSE
                IMINCT = 1
              ENDIF
*
              IF (NKEYCK.GE.IMINCT) THEN
                CALL CDCHKY (ITIME, KEYS)
                IF (IUSECT.NE.ICUR.OR.MNKYCT.NE.KEY1)          GO TO 80
              ENDIF
   75       CONTINUE
*
   80       CALL RZCDIR (PATHN, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 31
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK : '//
     +        'Path name '//PATHN//' from node bank illegal'')', IQ, 0)
#endif
              GO TO 998
            ENDIF
            NKEYCK = IQUEST(7)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            IF (IUSECT.EQ.ICUR.AND.KEY1.EQ.MNKYCT.AND.
     +          JBIT(KEY6CT,JIGNCD).EQ.0)                      GO TO 998
*
          ENDIF
*
        ENDIF
*
*  *    Data should be refreshed
*  *    Check which keys are to be used
*
   85   IF (IOPSCA.EQ.0)  THEN
          DO 90 I = 1, NWKYCK
            IF (IOKYCA(I).NE.0) THEN
              IF (I.EQ.IDHINS.OR.(I.GE.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD))
     +                                                              THEN
                KEYVCK(I) = KEYS(I)
              ELSE
                KEYVCK(I) = IQ(KOFUCD+LREFCD(2)+I)
              ENDIF
            ELSE
              KEYVCK(I) = 0
            ENDIF
   90     CONTINUE
        ELSE
          CALL UCOPY (IQ(KOFUCD+LREFCD(2)+1), KEYVCK, NWKYCK)
        ENDIF
*
*  *    Retrieve data
*
        IDTYP  = ICDTYP (LREFCD(2))
        IF (IOPVCA.NE.0.OR. JBIT(KEY6,JRZUCD).NE.0 . OR.
     +     (IDTYP.NE.2.AND.IDTYP.NE.3.AND.IDTYP.NE.5)) THEN
*
*  *      Variable data length - attach data to a new bank
*
          CALL MZDROP (IDIVCD, LREFCD(3), 'L')
          CALL CDKXIN (ITIME, IDIVCD, LBD(1), LBK(1), -KLDACD, NWKEY,
     +                 KEYVCK, IPREC, IRC)
          IF (IRC.NE.0)                                        GO TO 998
          LREFCD(3) = LBD(1)
          IF (IOPSCA.EQ.0) THEN
            IF (IQ(KOFUCD+LREFCD(2)+IDHKSN).NE.KEYVCK(IDHKSN)) IQ2 = 1
          ELSE
            IQ2 = 1
          ENDIF
*
        ELSE
*
*  *      Fixed length data - overwrite existing data
*
          IF (LAUXCL(7).NE.0)  THEN
            CALL MZDROP (IDISCD, LAUXCL(7), 'L')
            LAUXCL(7) = 0
          ENDIF
          NDAT   = IQ(KOFUCD+LREFCD(3)-1)
          CALL CDKXIN (ITIME, IDISCD, LAUXCL(7), LAUXCL(7), JBIAS,
     +                 NWKEY, KEYVCK, IPREC, IRC)
          IF (IRC.NE.0)                                        GO TO 998
          IF (IOPSCA.EQ.0) THEN
            IF (IQ(KOFUCD+LREFCD(2)+IDHKSN).NE.KEYVCK(IDHKSN)) IQ2 = 1
          ELSE
            IQ2 = 1
          ENDIF
#if defined(CERNLIB__P3CHILD)
          IF (IPASP3.EQ.1)                                     GO TO 200
#endif
*
          IF (LAUXCL(7).EQ.0) THEN
            IRC    = 36
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDCHCK : Ill'//
     +      'egal Bank address from CDKXIN'')', IARGCD, 0)
#endif
            GO TO 998
*
          ELSE
*
*  **       See if the old bank size is sufficient to store the data
*
            ND     = IQ(KOFUCD+LAUXCL(7)-1)
            IF (ND.GT.NDAT) THEN
*
*  *          Insufficient space
*
              CALL MZDROP (IDISCD, LAUXCL(7), 'L')
              IRC        = 37
              IQUEST(11) = NDAT
              IQUEST(12) = ND
#if defined(CERNLIB__DEBUG)
              IF (IDEBCD.GT.0) THEN
                CALL CDPRNT (LPRTCD, '(/,'' CDCHCK : Insufficient spa'//
     +               'ce '',I10,'' to store data - a minimum of '','//
     +               'I10,'' storage is needed'')', IQUEST(11), 2)
              ENDIF
#endif
              GO TO 998
*
            ELSE
*
*  *          Everything is OK
*
              CALL UCOPY (Q(KOFUCD+LAUXCL(7)+1), Q(KOFUCD+LREFCD(3)+1),
     +                    ND)
              CALL MZDROP (IDISCD, LAUXCL(7), 'L')
              IRC    = 0
*
            ENDIF
*
          ENDIF
*
        ENDIF
*
*  *    Overwrite keys in key-bank
*
  200   CALL UCOPY (KEYVCK(1), IQ(KOFUCD+LREFCD(2)+1), NWKYCK)
        IQ(KOFUCD+LREFCD(2)+NDK+MKYPRE) = IPREC
        IQ(KOFUCD+LREFCD(2)+NDK+MKYRID) =IQ(KOFUCD+LREFCD(2)+NDK+MKYRID)
     +                                  + 1
        IF (IOPSCA.EQ.0) THEN
          IF (IPRBCA+IPRECA.EQ.0) THEN
            IF (IHFLCD.EQ.0) THEN
              KYEN   = 0
              DO 205 I = 1, NPARCD
                IF (KYEN.EQ.0) THEN
                  IF (IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I).LT.KYENCD(I))
     +                                                        KYEN = -1
                  IF (IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I).GT.KYENCD(I))
     +                                                        KYEN =  1
                ENDIF
  205         CONTINUE
              DO 210 I = 1, NPARCD
                IF (KYEN.LT.0) THEN
                  IQ(KOFUCD+LREFCD(2)+NWKYCK+I) =
     +                      IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I)
                ELSE
                  IQ(KOFUCD+LREFCD(2)+NWKYCK+I) = KYENCD(I)
                ENDIF
  210         CONTINUE
            ELSE
              DO 215 I = 1, NPARCD
                IQ(KOFUCD+LREFCD(2)+NWKYCK+I) =
     +          MIN (IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I), KYENCD(I))
  215         CONTINUE
            ENDIF
          ELSE
            DO 220 I = 1, NPARCD-1
              IQ(KOFUCD+LREFCD(2)+NWKYCK+I) =
     +          IQ(KOFUCD+LREFCD(2)+NOF1CK+2*I-1)
  220       CONTINUE
            IQ(KOFUCD+LREFCD(2)+NWKYCK+NPARCD) =
     +        IQ(KOFUCD+LREFCD(2)+NOF1CK+2*NPARCD-1) + 1
          ENDIF
        ENDIF
*
      ENDIF
*
  998 IQUEST(2) = IQ2
*                                                             END CDCHCK
  999 END
