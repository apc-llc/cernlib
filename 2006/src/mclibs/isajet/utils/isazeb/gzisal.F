*
* $Id: gzisal.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisal.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     get pointer to first ISAL
C-   Returned value  : pointer
C-
C-   Created  17-AUG-1989   Serban Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISAL
#include "zebcom.inc"
#include "izisal.inc"
C----------------------------------------------------------------------
C
      IF(LISAE.GT.0) GZISAL=LQ(LISAE-IZISAL)
  999 RETURN
      END
