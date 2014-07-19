*
* $Id: cdcmpz.F,v 1.1.1.1 1996/02/28 16:24:20 mclareni Exp $
*
* $Log: cdcmpz.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCMPZ (LB, IADS, IER, IRC)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDCMPZ (LB, IADS*, IER*, IRC*)                          *
*                                                                      *
*   Compresses data bank defined by LB in the Data Base                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LB       Address of the bank to be compressed                    *
*     IADS     Address of the compressed bank as LAUXCL(IADS)          *
*     IER      Error Code : 0    No error                              *
*                           1    Compression does not result in gaining*
*                                space                                 *
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
#include "hepdb/czpack.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       LB(9)
*
*     ------------------------------------------------------------------
*
* *** Data type ?
*
      LREFCD(5) = LB(1)
      IDTYP  = ICDTYP (LREFCD(5))
      NDA    = IQ(KOFUCD+LREFCD(5)-1)
      ND2    = NDA - 2
      CALL CDBANK (IDISCD, LAUXCL(IADS), LAUXCL(IADS), JBIAS, 'AUX1', 0,
     +             0, NDA, IDTYP, 0, IRC)
      IF (IRC.NE.0)   GO TO 999
      LAUX1  = LAUXCL(IADS)
*
      IF (IDTYP.EQ.3) THEN
*
*  **   Compress non-0 real data
*
        CALL CDFZIN (Q(KOFUCD+LREFCD(5)+3), ND2, Q(KOFUCD+LAUX1+1), NDO,
     +               PRECCZ, IER)
        IF (IER.NE.0) THEN
          CALL MZDROP (IDISCD, LAUX1, ' ')
          GO TO 999
        ENDIF
*
*  **   Copy compressed data into reduced array with appropriate header
*
        ND3    = NDO + 3
        CALL CDBANK (IDISCD, LAUXCL(IADS+1), LAUXCL(IADS+1), JBIAS,
     +               'AUX2', 0, 0, ND3, IDTYP, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        LAUX1  = LAUXCL(IADS)
        LAUX2  = LAUXCL(IADS+1)
*
        Q(KOFUCD+LAUX2+1) = ND2
        Q(KOFUCD+LAUX2+2) = PRECCZ
        Q(KOFUCD+LAUX2+3) = NDO
        CALL UCOPY (Q(KOFUCD+LAUX1+1), Q(KOFUCD+LAUX2+4), NDO)
        CALL MZDROP (IDISCD, LAUX1, ' ')
        LAUXCL(IADS) = LAUXCL(IADS+1)
        LAUXCL(IADS+1) = 0
*
       ELSE IF (IDTYP.EQ.2.OR.IDTYP.EQ.5) THEN
*
*  **   Compress non-0 integer data
*
        CALL CDIZIN (IQ(KOFUCD+LREFCD(5)+3), ND2, IQ(KOFUCD+LAUX1+1),
     +               NDO, PRECCZ, IER)
        IF (IER.NE.0) THEN
          CALL MZDROP (IDISCD, LAUX1, ' ')
          GO TO 999
        ENDIF
*
*  **   Copy compressed data into reduced array with appropriate header
*
        ND3    = NDO + 3
        CALL CDBANK (IDISCD, LAUXCL(IADS+1), LAUXCL(IADS+1), JBIAS,
     +               'AUX2', 0, 0, ND3, IDTYP, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        LAUX1  = LAUXCL(IADS)
        LAUX2  = LAUXCL(IADS+1)
*
        IQ(KOFUCD+LAUX2+1) = ND2
        IQ(KOFUCD+LAUX2+2) = PRECCZ
        IQ(KOFUCD+LAUX2+3) = NDO
        CALL UCOPY (IQ(KOFUCD+LAUX1+1), IQ(KOFUCD+LAUX2+4), NDO)
        CALL MZDROP (IDISCD, LAUX1, ' ')
        LAUXCL(IADS) = LAUXCL(IADS+1)
        LAUXCL(IADS+1) = 0
*
      ELSE
*
*  **   Nonstandard Data type
*
        IER    = 5
        CALL MZDROP (IDISCD, LAUX1, ' ')
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.1) CALL CDPRNT (LPRTCD, '(/,'' CDCMPZ : Data-Ty'//
     +  'pe'',I4,'' incompatible for packing'')', IDTYP, 1)
#endif
        GO TO 999
      ENDIF
*
      IER    = 0
*                                                             END CDCMPZ
  999 END
