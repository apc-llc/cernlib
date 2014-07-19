*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:53:49 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:49  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    KERNGT          .VERSION KERNTMO  1.02  891211
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
#include "kerntmo/TITLE.kerntmo"
