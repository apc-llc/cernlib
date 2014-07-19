*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:50:58 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:58  mclareni
* Kernlib
*
*
#include "kernamx/pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    GSHOW           .VERSION KERNAMX  1.00  910718
C ORIG. 12/12/83  JZ
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
#include "kernamx/TITLE.kernamx"
