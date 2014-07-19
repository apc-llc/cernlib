*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    KERNGT          .VERSION KERNALT  1.08  891207
C ORIG.  16/03/89
C
C--   Store Pam title into /SLATE/ for printing by GSHOWF
 
      COMMON /SLATE/ MCH(40)
 
C-          GSHOW is the old name for KERNGT
      ENTRY GSHOW (LUN)
 
      CALL UCTOH1   (
     + CPP_TITLE_CH
     + // '        '
     +,  MCH, 40)
      CALL GSHOWF (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_KERNGT
#undef CERNLIB_TCGEN_KERNGT
#endif
#ifdef CERNLIB_TCGEN_GSHOW
#undef CERNLIB_TCGEN_GSHOW
#endif
#include "kernalt/TITLE.kernalt"
