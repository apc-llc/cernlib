*
* $Id: gzisaj.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisaj.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISAJ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISAJ
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISAJ
#include "zebcom.inc"
#include "izisaj.inc"
C----------------------------------------------------------------------
C
      GZISAJ=0
      IF(LISAE.NE.0)  GZISAJ=LQ(LISAE-IZISAJ)
C
      RETURN
      END
