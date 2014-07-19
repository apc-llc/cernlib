*
* $Id: jumpx2.F,v 1.1.1.1 1996/02/15 17:53:23 mclareni Exp $
*
* $Log: jumpx2.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE JUMPX2 (IPARA,IPARB)
C
C CERN PROGLIB# Z042    JUMPX2          .VERSION KERNIBM  2.28  891004
C ORIG. 21/04/88 JZ
C MOD.  12/09/89 JZ, add entries JUMPX1, JUMPX0
C
C-    To transfer to the user routine TARGET (say) with 2 parameters
C-    three steps are needed :
C
C- 1) EXTERNAL TARGET              to get the address of TARGET
C-    IADR = JUMPAD (TARGET)
C
C- 2) CALL JUMPST (IADR)           to set the tranfer address
C
C- 3) CALL JUMPX2 (par1,par2)      to transfer
C
      DIMENSION IPARA(9), IPARB(9)
C
      CALL  JUMPY2 (JPOINT,IPARA,IPARB)
      RETURN
C
      ENTRY JUMPX1 (IPARA)
      CALL  JUMPY1 (JPOINT,IPARA)
      RETURN
C
      ENTRY JUMPX0
      CALL  JUMPY0 (JPOINT)
      RETURN
C
      ENTRY JUMPST (JADR)
      JPOINT = JADR
      RETURN
      END
C-----------------------------------------------------
      SUBROUTINE JUMPY2 (TARSUB,IPARA,IPARB)
      DIMENSION IPARA(9), IPARB(9)
      EXTERNAL  TARSUB
C
      CALL  TARSUB (IPARA,IPARB)
      RETURN
C
      ENTRY JUMPY1 (TARSUB,IPARA)
      CALL  TARSUB (IPARA)
      RETURN
C
      ENTRY JUMPY0 (TARSUB)
      CALL  TARSUB
      RETURN
      END
#ifdef CERNLIB_TCGEN_JUMPXN
#undef CERNLIB_TCGEN_JUMPXN
#endif
