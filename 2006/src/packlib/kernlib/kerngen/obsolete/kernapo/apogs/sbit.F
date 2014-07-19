*
* $Id: sbit.F,v 1.1.1.1 1996/02/15 17:51:05 mclareni Exp $
*
* $Log: sbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:05  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE SBIT (IT,IZW,IZP)
C
C CERN PROGLIB# M421    SBIT            .VERSION KERNAPO  1.00  831212
C ORIG. 12/12/83  JZ
C

      IZW = OR (AND(NOT(LSHFT(1,IZP-1)),IZW),
     +          LSHFT(AND(IT,1),IZP-1))
      RETURN
      END
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
