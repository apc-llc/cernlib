*
* $Id: isa_smear.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isa_smear.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISA_SMEAR(ITYP,ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   ITYP   = 1 for e.m., 2 for had. energy, 3 special region
C-   ETA    = eta for energy
C-   ENERGY = energy to be smeared
C-
C-   Output:
C-   ENERGY = smeared energy
C-
C-   ENTRY ISMRFL : fill ISMR bank with resolution parameters
C-
C-   Created  29-JUN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
      INTEGER ITYP
      INTEGER LISMR,LDISMR,I
      REAL    ENERGY,R1
      REAL    A(3),B(3),C(3)
C   e.m. resolution= 2% constant, 15%/sqrt(E), 30 MeV noise/tower
C   had. resolution= 3% constant, 50%/sqrt(E), 120 Mev noise/tower
C   special region = 80%/sqrt(E) in CC to EC transition region
      DATA A/.02,.03,.03/
      DATA B/.15,.50,.80/
      DATA C/.03,.12,.12/
C----------------------------------------------------------------------
C
C    smear energies by resolution
C
      CALL NORRAN(R1)
      ENERGY=ENERGY+R1*SQRT((A(ITYP)*ENERGY)**2+ENERGY*B(ITYP)**2
     &  +C(ITYP)**2)
C
      RETURN
C
      ENTRY ISMRFL
C
        CALL BKISMR(LISMR)
        DO I=1,3
          LDISMR=LISMR+(I-1)*3
          Q(LDISMR+1)=A(I)
          Q(LDISMR+2)=B(I)
          Q(LDISMR+3)=C(I)
        ENDDO
C
  999 RETURN
      END
