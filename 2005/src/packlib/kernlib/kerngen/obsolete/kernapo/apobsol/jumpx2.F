*
* $Id: jumpx2.F,v 1.1.1.1 1996/02/15 17:51:11 mclareni Exp $
*
* $Log: jumpx2.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:11  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE JUMPX2 (IPARA,IPARB)
C
C CERN PROGLIB# Z042    JUMPX2          .VERSION KERNAPO  1.18  890929
C ORIG. 21/04/88 JZ
C MOD.  12/09/89 JZ, add entries JUMPX1, JUMPX0
C
C-    To transfer to the user routine TARGET (say) with 2 parameters
C-    three steps are needed :

C- 1) EXTERNAL TARGET              to get the address of TARGET
C-    IADR = JUMPAD (TARGET)

C- 2) CALL JUMPST (IADR)           to set the tranfer address

C- 3) CALL JUMPX2 (par1,par2)      to transfer

      DIMENSION IPARA(9), IPARB(9)

      INTEGER   JPOINT
      POINTER / JPOINT / TARGET
      SAVE      JPOINT

      CALL  JUMPY2 (TARGET,IPARA,IPARB)
      RETURN

      ENTRY JUMPX1 (IPARA)
      CALL  JUMPY1 (TARGET,IPARA)
      RETURN

      ENTRY JUMPX0
      CALL  JUMPY0 (TARGET)
      RETURN

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
