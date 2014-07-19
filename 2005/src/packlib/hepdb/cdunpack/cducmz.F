*
* $Id: cducmz.F,v 1.1.1.1 1996/02/28 16:24:35 mclareni Exp $
*
* $Log: cducmz.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUCMZ (LB, IADS, IRC)
*     =================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUCMZ (LB, IADS*, IRC*)                                *
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
*               = 82 : Illegal Data Type                               *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#include "hepdb/czpack.inc"
      PARAMETER       (JBIAS = 2)
      DIMENSION       LB(9)
*
*     ------------------------------------------------------------------
*
      LREFCD(7) = LB(1)
      IDTYP     = ICDTYP (LREFCD(7))
*
      IF (IDTYP.EQ.3) THEN
        NDI    = IQ(KOFUCD+LREFCD(7)-1) - 3
        NDO    = Q(KOFUCD+LREFCD(7)+1)
        PRECCZ = Q(KOFUCD+LREFCD(7)+2)
        ND2    = NDO + 2
        CALL CDBANK (IDISCD, LAUXCL(IADS), LAUXCL(IADS), JBIAS, 'AUX3',
     +               0, 0, ND2, IDTYP, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        LAUX3  = LAUXCL(IADS)
        LAU33  = LAUX3 + 3
        IF (NDI.LE.0) THEN
          CALL VZERO (Q(KOFUCD+LAU33), NDO)
        ELSE
          CALL CDUPFZ (Q(KOFUCD+LREFCD(7)+4), NDI, Q(KOFUCD+LAU33), NDO)
        ENDIF
      ELSE IF (IDTYP.EQ.2.OR.IDTYP.EQ.5) THEN
        NDI    = IQ(KOFUCD+LREFCD(7)-1) - 3
        NDO    = IQ(KOFUCD+LREFCD(7)+1)
        PRECCZ = IQ(KOFUCD+LREFCD(7)+2)
        ND2    = NDO + 2
        CALL CDBANK (IDISCD, LAUXCL(IADS), LAUXCL(IADS), JBIAS, 'AUX3',
     +               0, 0, ND2, IDTYP, 0, IRC)
        IF (IRC.NE.0) GO TO 999
        LAUX3  = LAUXCL(IADS)
        LAU33  = LAUX3 + 3
        IF (NDI.LE.0) THEN
          CALL VZERO (IQ(KOFUCD+LAU33), NDO)
        ELSE
          CALL CDUPIZ (IQ(KOFUCD+LREFCD(7)+4),NDI, IQ(KOFUCD+LAU33),NDO)
        ENDIF
      ELSE
        IRC        = 82
        IQUEST(11) = IDTYP
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDUCMZ : Data-ty'//
     +     'pe '',I4,'' illegal for uncompression'')', IQUEST(11), 1)
#endif
      ENDIF
*                                                             END CDUCMZ
  999 END
