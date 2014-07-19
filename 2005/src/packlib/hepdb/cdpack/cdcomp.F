*
* $Id: cdcomp.F,v 1.1.1.1 1996/02/28 16:24:20 mclareni Exp $
*
* $Log: cdcomp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCOMP (LSUP, LUPDA, KEYS, IRC)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCOMP (LSUP, LUPDA*, *KEYS*, IRC*)                     *
*                                                                      *
*   Compresses data bank defined by LSUP in the Data Base              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LSUP     Address of the bank to be compressed                    *
*     LUPDA    Address of the compressed bank                          *
*     KEYS     Vector of keys; Key 2 on return will contain the        *
*              pointer to the Key 1 of the object with reference       *
*              to which the current object has been updated            *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDENFZ, CDENTB, CDKOUT, CDPART                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                      Could be set to nonzero by some routines called *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/czpack.inc"
      PARAMETER       (JBIAS=2, MXNEI=5)
      DIMENSION       LSUP(9), LUPDA(9), KEYS(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      KEYS(IDHPTR) = 0
      IADS         = 1
      LREFCD(3)    = LSUP(1)
*
* *** If data is of mixed type, data cannot be compressed !
*
      ITYP   = ICDTYP (LREFCD(3))
      IF (ITYP.NE.2.AND.ITYP.NE.3)  THEN
        LUPDA(1) = LREFCD(3)
        GO TO 999
      ENDIF
*
* *** Compress the data
*
      IF (PACKCZ) THEN
        CALL CDCMPZ (LSUP(1), IADS, IER, IRC)
      ELSE
        CALL CDCMPR (LSUP(1), IADS, IER, IRC)
      ENDIF
      IF (IRC.NE.0)                   GO TO 999
*
* *** If IER .ne. 0 ----> Data cannot be compressed !
*
      IF (IER.EQ.0)  THEN
        LUPDA(1) = LAUXCL(IADS)
      ELSE
        LUPDA(1) = LREFCD(3)
        GO TO 999
      ENDIF
*
      IF (IOPDCA.EQ.0)                GO TO 999
      IF (NKEYCK.EQ.0)                GO TO 999
*
* *** Can we compress better by taking the difference of the present and
* ***   neighbouring objects ?
*
      ISTP   = NWKYCK + 1
*
      LREFCD(4) = LUPDA(1)
      MINWD  = IQ(KOFUCD+LREFCD(4)-1)
      IADS   = 9
      NEIGH  = 0
      DO 50 I = 1, NKEYCK
        J      = NKEYCK + 1 - I
        IPNT   = KOFSCD + LCDRCD + IKDRCD + (J-1) * ISTP
        KEY2   = IQ(IPNT+IDHPTR)
        IF (KEY2.NE.0)                GO TO 50
        KEY6   = IQ(IPNT+IDHFLG)
        IF (JBIT(KEY6,JRZUCD).NE.0)   GO TO 50
        IF (IOPFCA.NE.0) THEN
          CALL CDKEYR (J, NWKYCK, KEYNCK)
          DO 10 K = NSYSCK+1, NWKYCK
            IF (KEYS(K).NE.KEYNCK(K)) GO TO 50
   10     CONTINUE
        ENDIF
        NEIGH  = NEIGH + 1
        IF (NEIGH.GT.MXNEI)           GO TO 60
*
        IF (LSTRCL(4).NE.0) CALL MZDROP (IDISCD, LSTRCL(4), 'L')
        LSTRCL(4) = 0
        CALL CDRZIN (IDISCD, LSTRCL(4), JBIAS, J, ICYCL, ' ', IRC)
        IF (IRC.NE.0)  THEN
          IF (LSTRCL(4).NE.0) CALL MZDROP (IDISCD, LSTRCL(4), 'L')
          GO TO 50
        ENDIF
*
        CALL CDKEYR (J, NWKYCK, KEYNCK)
        LREFCL(7) = LSTRCL(4)
*
*  **   Uncompress ?
*
        IF (IQ(KOFUCD+LREFCL(7)+1).NE.0)  THEN
          CALL CDUNCP (LSTRCL(4), LREFCL(2), 0, IRC)
          CALL MZDROP (IDISCD, LSTRCL(4), 'L')
          IF (IRC.NE.0)               GO TO 999
          LREFCL(7) = LREFCL(2)
        ENDIF
*
*  **   Look if the data are similar - take the most similar master
*
        ND1    = IQ(KOFUCD+LREFCD(3) - 1)
        ND2    = IQ(KOFUCD+LREFCL(7) - 1)
        IF (ND2.EQ.0) THEN
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDCOMP : The '//
     +    'Data Structure does not contain data'',/)', IARGCD, 0)
#endif
          CALL MZDROP (IDISCD, LREFCL(7), ' ')
          GO TO 50
        ENDIF
*
        IT1    = ICDTYP(LREFCD(3))
        IT2    = ICDTYP(LREFCL(7))
        IF (IT1.NE.IT2)  THEN
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.1) THEN
            IARGCD(1) = IT1
            IARGCD(2) = IT2
            CALL CDPRNT (LPRTCD, '(/,'' CDCOMP : The 2 Structures hav'//
     +           'e different data type '',2I4)', IARGCD, 2)
          ENDIF
#endif
          CALL MZDROP (IDISCD, LREFCL(7), ' ')
          GO TO 50
        ENDIF
        IF (ND1.NE.ND2) THEN
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.1) THEN
            IARGCD(1) = ND1
            IARGCD(2) = ND2
            CALL CDPRNT (LPRTCD, '(/,'' CDCOMP : The 2 Structures hav'//
     +      'e different # of data'',2I4)', IARGCD, 2)
          ENDIF
#endif
          CALL MZDROP (IDISCD, LREFCL(7), ' ')
          GO TO 50
        ENDIF
        IF (IT1.EQ.2.OR.IT1.EQ.5) THEN
          IQ(KOFUCD+LREFCL(7)+2) = IQ(KOFUCD+LREFCD(3)+2)
          DO 20 ID = 3, ND1
            IQ(KOFUCD+LREFCL(7)+ID) = IQ(KOFUCD+LREFCL(7)+ID)
     +                              - IQ(KOFUCD+LREFCD(3)+ID)
   20     CONTINUE
        ELSE
          Q(KOFUCD+LREFCL(7)+2) = Q(KOFUCD+LREFCD(3)+2)
          DO 30 ID = 3, ND1
             Q(KOFUCD+LREFCL(7)+ID) =  Q(KOFUCD+LREFCL(7)+ID)
     +                              -  Q(KOFUCD+LREFCD(3)+ID)
   30     CONTINUE
        ENDIF
*
   40   CONTINUE
        IF (PACKCZ) THEN
          CALL CDCMPZ (LREFCL(7), IADS, IER, IRC)
        ELSE
          CALL CDCMPR (LREFCL(7), IADS, IER, IRC)
        ENDIF
        IF (IRC.NE.0) THEN
          CALL MZDROP (IDISCD, LREFCL(7), ' ')
          GO TO 999
        ENDIF
        IF (IER.NE.0) THEN
          CALL MZDROP (IDISCD, LREFCL(7), ' ')
          GO TO 50
        ENDIF
*
        NWORD  = IQ(KOFUCD+LAUXCL(IADS)-1)
        IF (NWORD.GE.MINWD)  THEN
          CALL MZDROP (IDISCD, LREFCL(7),    ' ')
          CALL MZDROP (IDISCD, LAUXCL(IADS), ' ')
          GO TO 50
        ENDIF
        KEYS(IDHPTR)= KEYNCK(IDHKSN)
        MINWD  = NWORD
        CALL MZDROP (IDISCD, LREFCL(7), ' ')
        CALL MZDROP (IDISCD, LREFCD(4), ' ')
        LREFCD(4) = LAUXCL(IADS)
        IF (IADS.EQ.1)  THEN
          IADS   = 9
        ELSE IF (IADS.EQ.9) THEN
          IADS   = 1
        ENDIF
   50 CONTINUE
   60 CONTINUE
      LUPDA(1) = LREFCD(4)
      IRC      = 0
*                                                             END CDCOMP
  999 END
