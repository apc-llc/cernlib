*
* $Id: gzisaq.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisaq.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISAQ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISAQ
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISAQ
#include "zebcom.inc"
#include "izisaq.inc"
C----------------------------------------------------------------------
C
      GZISAQ=0
      IF(LISAE.NE.0)  GZISAQ=LQ(LISAE-IZISAQ)
C
      RETURN
      END
