*
* $Id: sbit.F,v 1.1.1.1 1996/02/15 17:52:30 mclareni Exp $
*
* $Log: sbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:30  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
        SUBROUTINE SBIT (IA,IX,J)
C
C CERN PROGLIB# M421    SBIT            .VERSION KERNCVX  1.00  890315
C ORIG. 01/01/89  AGA+MAV, Convex
C

      IF (BTEST(IA,0)) THEN
          IX=IBSET(IX,J-1)
        ELSE
          IX=IBCLR(IX,J-1)
        ENDIF
      RETURN
      END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
