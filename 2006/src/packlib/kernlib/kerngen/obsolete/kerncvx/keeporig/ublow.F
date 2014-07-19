*
* $Id: ublow.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: ublow.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE UBLOW (VM, V1, NCH)
C
C CERN PROGLIB# M409    UBLOW           .VERSION KERNCVX  1.00  890315
C ORIG. 01/01/89  AGA+MAV, Convex
C
C DISPERSES THE STRING OF NCH HOLLERITH CHARACTERS FROM VM INTO VI
C
      CHARACTER*1 VM(*)
      CHARACTER*8 V1(*)

      DO I=1,NCH
        V1(I) = '        '
      ENDDO

      DO I=1,NCH
        V1(I)(1:1)=VM(I)
      ENDDO
      RETURN
      END
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
