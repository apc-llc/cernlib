*
* $Id: kaxend.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxend.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXEND
*
*.....ABANDON PROCESSING
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
*
*-----------------------------------------------------------------------
*
      CALL KAXMSG(LUNERR,'CRNKA999 KAXEND: PROCESSING TERMINATED OWING
     +                    TO THE ABOVE CONDITION')
*
      CALL KASTOP
*
      STOP 999
*
      END
