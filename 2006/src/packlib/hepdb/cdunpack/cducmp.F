*
* $Id: cducmp.F,v 1.1.1.1 1996/02/28 16:24:35 mclareni Exp $
*
* $Log: cducmp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUCMP (LB, IADS, IRC)
*     =================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUCMP (LB, IADS*, IRC*)                                *
*                                                                      *
*   Uncompresses data bank defined by LB in the Data Base              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LB       Address of the bank to be uncompressed                  *
*     IADS     Address of the uncompressed bank as LAUXCL(IADS)        *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDUNCP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 81 : Precision is not correctly given                *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
      PARAMETER       (JBIAS = 2)
      DIMENSION       LB(9)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      LREFCD(7) = LB(1)
      NDI    = IQ(KOFUCD+LREFCD(7)-1) - 3
      LB3    = LREFCD(7) + 3
      NAU    = JBYT (IQ(KOFUCD+LB3), 1, 26)
      LBITL  = JBYT (IQ(KOFUCD+LB3), 27, 5)
      IVM    = IQ(KOFUCD+LREFCD(7)+2)
      LB1    = LREFCD(7) + 1
      NDO    = IQ(KOFUCD+LB1)/10000
      IDT    = MOD (IQ(KOFUCD+LB1), 10000)
      IPREC  = MOD (IDT, 1000)
      IDT    = IDT/1000
      IPREC  = IPREC - 100
*
      ND2    = NDO + 2
      CALL CDBANK (IDISCD, LAUXCL(IADS), LAUXCL(IADS), JBIAS, 'AUX3', 0,
     +             0, ND2, 2, 0, IRC)
      IF (IRC.NE.0)   GO TO 999
*
      LAUX3  = LAUXCL(IADS)
      LAU33  = LAUX3 + 3
      CALL CDUPCK (IQ(KOFUCD+LREFCD(7)+4), NDI, NDO, NAU, LBITL,
     +             IQ(KOFUCD+LAU33))
*
      LAU32  = KOFUCD + LAUX3 + 2
      IF (IVM.GT.0) THEN
        DO 10 I = 1, NDO
          IQ(LAU32+I) = IQ(LAU32+I) - IVM
   10   CONTINUE
      ENDIF
*
* *** Precision
*
      IF (IPREC.LT.-99.OR.IPREC.GT.99) THEN
        IRC        = 81
        IQUEST(11) = IPREC
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUCMP : Precisi'//
     +     'on is not correctly given -'',I8,'' no uncompression'')',
     +     IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
      IPR1   = 0
      IPR2   = 0
      IF (IPREC.GT.0) IPR2 = IPREC
      IF (IPREC.LT.0) IPR1 = -IPREC
*
      IF (IDT.EQ.3) THEN
        CALL VFLOAT (IQ(KOFUCD+LAU33), Q(KOFUCD+LAU33), NDO)
        IF (IPR1.GT.0) THEN
          XMULT  = 10.**IPR1
          CALL VSCALE (Q(KOFUCD+LAU33), XMULT, Q(KOFUCD+LAU33), NDO)
        ELSE IF (IPR2.GT.0) THEN
          XMULT  = 1./10.**IPR2
          CALL VSCALE (Q(KOFUCD+LAU33), XMULT, Q(KOFUCD+LAU33), NDO)
        ENDIF
*
        CALL CDBANK (IDISCD, LAUXCL(IADS-1), LAUXCL(IADS-1), JBIAS,
     +               'AUX4', 0, 0, ND2, 3, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        LAUX3  = LAUXCL(IADS)
        LAU33  = LAUX3 + 3
        LAUX4  = LAUXCL(IADS-1)
        Q(KOFUCD+LAUX4+2) = IPREC
        CALL UCOPY (Q(KOFUCD+LAU33), Q(KOFUCD+LAUX4+3), NDO)
        CALL MZDROP (IDISCD, LAUX3, ' ')
      ELSE IF (IDT.EQ.2 .OR. IDT.EQ.5)  THEN
        IF (IPR1.GT.0) THEN
          IMULT  = 10**IPR1
          DO 20 I = 1, NDO
            IQ(LAU32+I) = IQ(LAU32+I) * IMULT
   20     CONTINUE
        ELSE IF (IPR2.GT.0) THEN
          XMULT  = 1./10**IPR2
          DO 30 I = 1, NDO
            IQ(LAU32+I) = IQ(LAU32+I) * XMULT
   30     CONTINUE
        ENDIF
*
        CALL CDBANK (IDISCD, LAUXCL(IADS-1), LAUXCL(IADS-1), JBIAS,
     +               'AUX4', 0, 0, ND2, IDT, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        LAUX3  = LAUXCL(IADS)
        LAU33  = LAUX3 + 3
        LAUX4  = LAUXCL(IADS-1)
        IQ(KOFUCD+LAUX4+2) = IPREC
        CALL UCOPY (Q(KOFUCD+LAU33), IQ(KOFUCD+LAUX4+3), NDO)
        CALL MZDROP (IDISCD, LAUX3, ' ')
      ENDIF
*                                                             END CDUCMP
  999 END
