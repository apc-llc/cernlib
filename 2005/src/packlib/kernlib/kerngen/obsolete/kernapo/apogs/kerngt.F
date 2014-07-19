*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:51:01 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:01  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    KERNGT          .VERSION KERNAPO  1.16  890321
C ORIG. 16/03/89  JZ
C
C--   Store Pam title into /SLATE/ for printing by GSHOWF
C
      COMMON /SLATE/ MCH(40)

      CALL UCTOH1 (
     + CPP_TITLE_CH
     + // '        '
     +,  MCH,40)
      CALL GSHOWF (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_KERNGT
#undef CERNLIB_TCGEN_KERNGT
#endif
#include "kernapo/TITLE.kernapo"
