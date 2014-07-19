*
* $Id: gziscm.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gziscm.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISCM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  : pointer to ISCM bank (ISAJET commands)
C-
C-   Created   5-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISCM
#include "zebcom.inc"
#include "iziscm.inc"
C----------------------------------------------------------------------
C
      GZISCM=0
      IF(LISAB.GT.0) GZISCM=LQ(LISAB-IZISCM)
  999 RETURN
      END
