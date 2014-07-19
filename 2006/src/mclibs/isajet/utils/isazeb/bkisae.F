*
* $Id: bkisae.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkisae.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISAE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK ISAE BANK
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  31-JAN-1991   Serban Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER IOISAE
      LOGICAL FIRST
#include "zebcom.inc"
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN              ! only for first event
        CALL MZFORM('ISAE','10I 6F 2D',IOISAE)
        FIRST=.FALSE.
      ENDIF
C
C  create ZEBRA bank ISAE (main supporting bank)
C
      CALL MZBOOK(IXMAIN,LISAE,LISAE,1,
     $            'ISAE',9,9,18,IOISAE,-1)

  999 RETURN
      END
