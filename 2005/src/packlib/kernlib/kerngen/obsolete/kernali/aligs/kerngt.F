*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:50:51 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:51  mclareni
* Kernlib
*
*
#include "kernali/pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB#         KERNGT          .VERSION KERNALI  1.00  900919
C ORIG.  3/02/89  K.M.STORR
C
C--   STORE PAM TITLE INTO /SLATE/ FOR PRINTING BY GSHOWF
 
      COMMON /SLATE/ ISLATE(40)
 
      CALL UCTOH1 (
     + CPP_TITLE_CH
     + // '        '
     +,            ISLATE, 40 )
      CALL GSHOWF (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_KERNGT
#undef CERNLIB_TCGEN_KERNGT
#endif
#include "kernali/TITLE.kernali"
