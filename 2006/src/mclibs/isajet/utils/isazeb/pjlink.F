*
* $Id: pjlink.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pjlink.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PJLINK (SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activate/de-activate PJET temporary link
C-   area.
C-
C-   Inputs  : SWITCH   [I]     1 -- Activate link area
C-                              0 -- De-activate link area
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER SWITCH
#include "zebcom.inc"
#include "lkpjet.inc"
C----------------------------------------------------------------------
      IF ( SWITCH .EQ. 1 ) THEN
        CALL MZLINT (IXMAIN,'LKPJET',JPJET,KPJET(MXPJET),JPJET)
      ELSE
        JPJET(1) = 0                    ! De-activate link area
      ENDIF
  999 RETURN
      END
