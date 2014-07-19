*
* $Id: sbit.F,v 1.1.1.1 1996/02/15 17:52:52 mclareni Exp $
*
* $Log: sbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:52  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE SBIT (IT,IW,JPOS)
C
C CERN PROGLIB# M421    SBIT            .VERSION KERNHYW  1.03  841013
C ORIG. 13/10/84 JZ
C
      JPH = 36 - JPOS
      FLD(JPH,1,IW) = IT
      RETURN
      END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
*      ==================================================
#include "qcardl.inc"
