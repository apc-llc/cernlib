*
* $Id: jumpx2.F,v 1.1.1.1 1996/02/15 17:54:21 mclareni Exp $
*
* $Log: jumpx2.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:21  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE JUMPX2 (IPARA,IPARB)
C
C CERN PROGLIB# Z042    JUMPX2          .VERSION KERNSUN  1.02  891025
C ORIG. 22/09/88 JZ
C MOD.  12/09/89 JZ, add entries JUMPX1, JUMPX0
C
C-    to transfer

      DIMENSION IPARA(9), IPARB(9)
      POINTER  (JPOINT, TARGET)
      SAVE      JPOINT

      CALL  JUMPY2 (TARGET,IPARA,IPARB)
      RETURN

      ENTRY JUMPX1 (IPARA)
      CALL  JUMPY1 (TARGET,IPARA)
      RETURN

      ENTRY JUMPX0
      CALL  JUMPY0 (TARGET)
      RETURN

C-    to set the tranfer address
      ENTRY JUMPST (JADR)
      JPOINT = JADR
      RETURN
      END
C-----------------------------------------------------
      SUBROUTINE JUMPY2 (TARSUB,IPARA,IPARB)
      DIMENSION IPARA(9), IPARB(9)
      EXTERNAL  TARSUB

      CALL  TARSUB (IPARA,IPARB)
      RETURN

      ENTRY JUMPY1 (TARSUB,IPARA)
      CALL  TARSUB (IPARA)
      RETURN

      ENTRY JUMPY0 (TARSUB)
      CALL  TARSUB
      RETURN
      END
#ifdef CERNLIB_CCGEN_JUMPXN
#undef CERNLIB_CCGEN_JUMPXN
#endif
#ifdef CERNLIB_TCGEN_JUMPXN
#undef CERNLIB_TCGEN_JUMPXN
#endif
