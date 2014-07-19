*
* $Id: ismear.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: ismear.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISMEAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Smear pseudo-calorimeter ISCL bank
C-     store smearing parameters in bank ISMR
C-
C-   Created  18-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "iziscl.inc"
      INTEGER LISAC,GZISAC,LISMR,GZISMR,LISCL,ITYP
      REAL    EEM,EHAD,ABSETA
C----------------------------------------------------------------------
      IF ( GZISMR().EQ.0 ) THEN
        LISAC=GZISAC()
        IF(LISAC.EQ.0)  THEN ! If pseudo-calorimeter data not available
          CALL ISACFL        ! make it
        ELSE
C         remake the ISAC bank if it has no link for ISMR
          IF(IQ(LISAC-3).LT.2) THEN
            CALL MZDROP(IXCOM,LISAC,' ')
            CALL ISACFL
          ENDIF
        ENDIF
C
        LISAC=GZISAC()
        LISCL=LISAC-IZISCL
  400   LISCL=LQ(LISCL)         !  Loop over all non-zero cells
        IF(LISCL.GT.0.) THEN
          ABSETA=ABS(Q(LISCL+10))
          EEM=Q(LISCL+3)
          EHAD=Q(LISCL+4)
          IF(EEM.GT.0) CALL ISA_SMEAR(1,EEM)
          IF(EHAD.GT.0) THEN
            ITYP=2
            IF(ABSETA.GT.1.0.AND.ABSETA.LT.1.5) ITYP=3
            CALL ISA_SMEAR(ITYP,EHAD)
          ENDIF
C
C            refill bank with smeared energies
          Q(LISCL+3)=EEM
          Q(LISCL+4)=EHAD
          GOTO 400
        ENDIF
C
C          book and fill ISMR
        CALL ISMRFL
C
      ENDIF
  999 RETURN
      END
