*
* $Id: cdcmpr.F,v 1.2 1996/04/25 10:23:46 cernlib Exp $
*
* $Log: cdcmpr.F,v $
* Revision 1.2  1996/04/25 10:23:46  cernlib
* Comment the include for zebra/q_cbyt.inc; this files no longer is in zebra
*
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCMPR (LB, IADS, IER, IRC)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCMPR (LB, IADS*, IER*, IRC*)                          *
*                                                                      *
*   Compresses data bank defined by LB in the Data Base                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LB       Address of the bank to be compressed                    *
*     IADS     Address of the compressed bank as LAUXCL(IADS)          *
*     IER      Error Code : 0    No error                              *
*                           1-3  As in CDFLIN/CDININ                   *
*                           4    Not enough precision for packing      *
*                           5    Nonstandard Data type                 *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDCOMP                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IER       =  0 : No error (see above)                            *
*     IRC       =  0 : No error                                        *
*                      Could be set to nonzero by some routines called *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       LB(9)
      SAVE            IWMX
      DATA            IWMX /1073741824/
#include "zebra/q_sbit.inc"
* Ignoring t=pass
*#include "zebra/q_cbyt.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
* *** Data type ?
*
      LREFCD(5) = LB(1)
      IDTYP  = ICDTYP(LREFCD(5))
      NDA    = IQ(KOFUCD+LREFCD(5)-1)
      CALL CDBANK (IDISCD, LAUXCL(IADS), LAUXCL(IADS), JBIAS, 'AUX1', 0,
     +             0, NDA, 2, 0, IRC)
      IF (IRC.NE.0) GO TO 999
      LAUX1  = LAUXCL(IADS)
      IF (IDTYP.EQ.3) THEN
*
*  **   Convert real to integer data and truncate according to precision
*
        CALL CDFLIN (Q(KOFUCD+LREFCD(5)+1), IQ(KOFUCD+LAUX1+1), NDA,
     +               IPREC, IER)
        IF (IER.NE.0) THEN
          CALL MZDROP (IDISCD, LAUX1, ' ')
          GO TO 999
        ENDIF
*
      ELSE IF (IDTYP.EQ.2.OR.IDTYP.EQ.5) THEN
*
*  **   Truncate integer data according to the given precision
*
        CALL CDININ (IQ(KOFUCD+LREFCD(5)+1), IQ(KOFUCD+LAUX1+1), NDA,
     +               IPREC, IER)
        IF (IER.NE.0) THEN
          CALL MZDROP (IDISCD, LAUX1, ' ')
          GO TO 999
        ENDIF
      ELSE
*
*  **   Nonstandard data type
*
        IER    = 5
        CALL MZDROP (IDISCD, LAUX1, ' ')
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDCMPR : Data-Ty'//
     +  'pe '',I4,'' incompatible for packing'')', IDTYP, 1)
#endif
        GO TO 999
      ENDIF
*
* *** Convert signed integer to positive integers
*
      ND2    = NDA - 2
      LAU12  = KOFUCD + LAUX1 + 2
      IVM    = IQ(LAU12+1)
      MVM    = IQ(LAU12+1)
      DO 10 I = 2, ND2
        IF (IQ(LAU12+I).LT.IVM) IVM = IQ(LAU12+I)
        IF (IQ(LAU12+I).GT.MVM) MVM = IQ(LAU12+I)
   10 CONTINUE
*
      IF (MVM.GT.IWMX.OR.-IVM.GT.IWMX) THEN
        IER    = 4
        CALL MZDROP (IDISCD, LAUXCL(IADS),   ' ')
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDCMPR : Packing'//
     +  ' Bit Length is '',I4,'' - No Compression !'')', LBITL, 1)
#endif
        GO TO 999
      ENDIF
*
      IF (IVM.LT.0) THEN
        IVM    = - IVM
        DO 20 I = 1, ND2
          IQ(LAU12+I) = IQ(LAU12+I) + IVM
   20   CONTINUE
      ELSE
        IVM    = 0
      ENDIF
*
* *** Pack the positive integers into bit-string choosing optimum
* *** bit-length
*
      ND3    = ND2 + 3
      CALL CDBANK (IDISCD, LAUXCL(IADS+1), LAUXCL(IADS+1), JBIAS, 'AUX2'
     +           , 0, 0, ND3, 1, 0, IRC)
      IF (IRC.NE.0) GO TO 999
      CALL CDBANK (IDISCD, LSTRCL(5), LSTRCL(5), JBIAS, 'WKSP', 0, 0
     +           , ND3, 1, -1, IRC)
      IF (IRC.NE.0) GO TO 999
      LAUX1  = LAUXCL(IADS)
      LAU13  = LAUX1 + 3
      LAUX2  = LAUXCL(IADS+1)
      CALL CDPACK (IQ(KOFUCD+LAU13), ND2, NDO, NAU, LBITL
     +           , IQ(KOFUCD+LAUX2+4), IQ(KOFUCD+LSTRCL(5)+1))
      CALL MZDROP (IDISCD, LSTRCL(5), ' ')
*
      IF (LBITL.EQ.32) THEN
        IER    = 4
        CALL MZDROP (IDISCD, LAUXCL(IADS),   ' ')
        CALL MZDROP (IDISCD, LAUXCL(IADS+1), ' ')
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDCMPR : Packing'//
     +  ' Bit Length is '',I4,'' - No Compression !'')', LBITL, 1)
#endif
        GO TO 999
      ENDIF
*
      IPREC  = IPREC + 100
      IQ(KOFUCD+LAUX2+1) = 10000*ND2 + 1000*IDTYP + IPREC
      IQ(KOFUCD+LAUX2+2) = IVM
      LAU    = LAUX2 + 3
      IQ(KOFUCD+LAU) = NAU
      IQ(KOFUCD+LAU) = MCBYT (LBITL, 1, IQ(KOFUCD+LAU), 27, 5)
      IQ(KOFUCD+LAU) = MSBIT1 (IQ(KOFUCD+LAU), 32)
*
* *** Rebook AUX1 bank with reduced datalength
*
      ND3    = NDO + 3
      CALL MZDROP (IDISCD, LAUX1, ' ')
      CALL CDBANK (IDISCD, LAUXCL(IADS), LAUXCL(IADS), JBIAS, 'AU11', 0,
     +             0, ND3, 1, 0, IRC)
      IF (IRC.NE.0) GO TO 999
      LAUX1  = LAUXCL(IADS)
      LAUX2  = LAUXCL(IADS+1)
      CALL UCOPY (IQ(KOFUCD+LAUX2+1), IQ(KOFUCD+LAUX1+1), ND3)
      CALL MZDROP (IDISCD, LAUX2, ' ')
      IER    = 0
*                                                             END CDCMPR
  999 END
