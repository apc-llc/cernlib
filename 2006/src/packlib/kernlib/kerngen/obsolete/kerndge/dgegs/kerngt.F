*
* $Id: kerngt.F,v 1.1.1.1 1996/02/15 17:54:25 mclareni Exp $
*
* $Log: kerngt.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:25  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      SUBROUTINE KERNGT (LUN)
C
C CERN PROGLIB# Z001    KERNGT          .VERSION KERNDGE  1.07  891003
C ORIG. 16/03/89  JZ
C
C--   Store Pam title into /SLATE/ for printing by GSHOWF
C
      COMMON /SLATE/ MCH(40)
 
      CALL UBLOW (
     + 40H CPP_TITLE                 
     +,  MCH,40)
      CALL GSHOWF (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_KERNGT
#undef CERNLIB_TCGEN_KERNGT
#endif
#include "kerndge/TITLE.kerndge"
