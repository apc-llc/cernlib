*
* $Id: ubunch.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: ubunch.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE UBUNCH (V1, VM, NCH)
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNCVX  1.00  890315
C ORIG. 15/03/89  JZ, CERN
C
C  CONCENTRATES THE STRING OF HOLLERITH CHARACTERS FROM V1 INTO VM
C
      CHARACTER*1 VM(*)
      CHARACTER*8 V1(*)


      DO  14  J=1,NCH
   14 VM(J) = V1(J)(1:1)

C--                Blank-fill last incomplete word

      NTRAIL = MOD (NCH,8)
      IF (NTRAIL.EQ.0)       RETURN

      DO  17  J=NCH+1,NCH+8-NTRAIL
   17 VM(J) = ' '
      RETURN
      END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
