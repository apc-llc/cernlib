*
* $Id: cduncp.F,v 1.1.1.1 1996/02/28 16:24:36 mclareni Exp $
*
* $Log: cduncp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDUNCP (LC, LU, IK, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUNCP (LC, LU*, IK, IRC*)                              *
*                                                                      *
*   Uncompresses data bank defined by LC in the Data Base              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LC       Address of the bank to be uncompressed                  *
*     LU       Address of the uncompressed bank                        *
*     IK       If nonzero the Key 1 of the master bank with reference  *
*              to which the bank is to be updated                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCOMP, CDKXIN                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 83 : Data update but uncompreseed                    *
*               = 84 : The update structure has different number of    *
*                      data words                                      *
*               = 85 : No data in the structure                        *
*               = 86 : The update structure has different data type    *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/czpack.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      PARAMETER       (JBIAS = 2)
      DIMENSION       LC(9), LU(9), KEY(MXDMCK)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      LREFCD(5) = LC(1)
      ITU       = ICDTYP (LREFCD(5))
#if defined(CERNLIB__P3CHILD)
*
* *** Compression parameters are put by RZIN in IQUEST(96 .. 100)
*
      IF (ITU.EQ.5) THEN
        CALL UCOPY (IQUEST(96), IQ(KOFUCD+LREFCD(5)+1), 2)
      ENDIF
#endif
*
* *** Data uncompressed ?
*
      NWKEY  = MIN (NWKYCK, NSYSCK)
      IF (IQ(KOFUCD+LREFCD(5)+1).EQ.0) THEN
        IF (IK.NE.0) THEN
          IRC    = 83
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUNCP : Incom'//
     +       'patibility - Data is an update but uncompressed'')',IRC,0)
#endif
          GO TO 999
        ENDIF
        NDU    = IQ(KOFUCD+LREFCD(5)-1)
        CALL CDBANK (IDISCD, LU(1), LU(1), JBIAS, 'SAME', 0, 0, NDU,
     +               ITU, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        IF (ITU.EQ.3)  THEN
          CALL UCOPY (Q(KOFUCD+LREFCD(5)+1), Q(KOFUCD+LU(1)+1), NDU)
        ELSE
          CALL UCOPY (IQ(KOFUCD+LREFCD(5)+1), IQ(KOFUCD+LU(1)+1), NDU)
        ENDIF
        GO TO 999
      ENDIF
*
* *** Uncompress data
* *** Which packing mode ?
*
      CALL UCOPY (Q(KOFUCD+LREFCD(5)+3), IXX, 1)
#if defined(CERNLIB__P3CHILD)
      IF (ITU.EQ.5) IXX = IQUEST(98)
#endif
      PACKCZ = (JBIT (IXX, 32)) .EQ. 0
*
      IADS   = 5
*
      IF (PACKCZ) THEN
        CALL CDUCMZ (LREFCD(5), IADS, IRC)
        LU(1) = LAUXCL(IADS)
      ELSE
#if defined(CERNLIB__P3CHILD)
        CALL UCOPY  (IQUEST(96), IQ(KOFUCD+LC(1)+1), 3)
#endif
        CALL CDUCMP (LREFCD(5), IADS, IRC)
        LU(1) = LAUXCL(IADS-1)
      ENDIF
      IF (IRC.NE.0)         GO TO 999
*
      IF (IK.EQ.0)          GO TO 999
      IF (NKEYCK.EQ.0)      GO TO 999
*
      CALL MZDROP (IDISCD, LREFCD(5), ' ')
*
      LREFCD(5) = 0
      LREFCD(6) = LU(1)
      LC(1)     = 0
      CALL CDRZIN (IDISCD, LREFCD(5), JBIAS, IK, ICYCL, ' ', IRC)
      IF (IRC.NE.0)         GO TO 999
      LC(1)     = LREFCD(5)
#if defined(CERNLIB__P3CHILD)
*
* *** Compression parameters are put by RZIN in IQUEST(96 .. 100)
*
      ITU    = ICDTYP (LREFCD(5))
      IF (ITU.EQ.5) THEN
        CALL UCOPY (IQUEST(96), IQ(KOFUCD+LREFCD(5)+1), 2)
      ENDIF
      IF (IPASP3.GT.1)      GO TO 10
#endif
      CALL CDKEYR (IK, NWKEY, KEY)
   10 LCC    = LREFCD(5)
*
* *** Uncompress master ?
*
      IF (IQ(KOFUCD+LREFCD(5)+1).NE.0) THEN
        IADS   = 5
        IF (PACKCZ) THEN
          CALL CDUCMZ (LREFCD(5), IADS, IRC)
          LCC    = LAUXCL(IADS)
        ELSE
#if defined(CERNLIB__P3CHILD)
          CALL UCOPY  (IQUEST(96), IQ(KOFUCD+LC(1)+1), 3)
#endif
          CALL CDUCMP (LREFCD(5), IADS, IRC)
          LCC    = LAUXCL(IADS-1)
        ENDIF
      ENDIF
      IF (IRC.NE.0)         GO TO 999
*
* *** Update
* *** Look if the data are similar - STOP if they are not
*
      ND1    = IQ(KOFUCD+LREFCD(6) - 1)
      ND2    = IQ(KOFUCD+LCC - 1)
      IF (ND1.NE.ND2) THEN
        IRC        = 84
        IQUEST(11) = ND1
        IQUEST(12) = ND2
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUNCP : The 2 '//
     +     'structures have different number of data = '',2I4)',
     +     IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
      IF (ND1.EQ.0) THEN
        IRC    = 85
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUNCP : The dat'//
     +     'a structure does not have data'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
      IT1    = ICDTYP (LREFCD(6))
      IT2    = ICDTYP (LCC)
      IF (IT1.NE.IT2) THEN
        IRC        = 86
        IQUEST(11) = IT1
        IQUEST(12) = IT2
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUNCP : The 2 '//
     +     'Structures have different data type '',2I4)', IQUEST(11), 2)
#endif
        GO TO 999
      ENDIF
      IF (IT1.EQ.2.OR.IT1.EQ.5) THEN
        DO 20 ID = 3, ND1
          IQ(KOFUCD+LREFCD(6)+ID) = IQ(KOFUCD+LCC+ID)
     +                            - IQ(KOFUCD+LREFCD(6)+ID)
   20   CONTINUE
      ELSE
        DO 30 ID = 3, ND1
          Q(KOFUCD+LREFCD(6)+ID) = Q(KOFUCD+LCC+ID)
     +                           - Q(KOFUCD+LREFCD(6)+ID)
   30   CONTINUE
      ENDIF
   40 IF (LREFCD(5).NE.LCC) CALL MZDROP (IDISCD, LCC, ' ')
*                                                             END CDUNCP
  999 END
