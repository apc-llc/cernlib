*
* $Id: isa_weight.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isa_weight.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ISA_WEIGHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      return weight for ISAJET event
C-   Returned value  : weight (microbarns/event)
C-
C-   Created  30-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      REAL ISA_WEIGHT
#include "zebcom.inc"
C----------------------------------------------------------------------
C
      ISA_WEIGHT=0
      IF(LISAE.GT.0) ISA_WEIGHT=Q(LISAE+12)
  999 RETURN
      END
