*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:52:23 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    KERNGT          .VERSION KERNCVX  1.10  940228
C ORIG.  16/03/89
C
C--   Store Pam title into /SLATE/ for printing by GSHOWF

      COMMON /SLATE/ MCH(40)
      CHARACTER    VIDQQ*(*)
      PARAMETER   (VIDQQ = '@(#)' //
     + CPP_TITLE_CH
     + // '    '
     + // '>')

      CALL CLEFT (VIDQQ,1,0)
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
#include "TITLE.kerncvx"
