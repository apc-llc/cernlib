*
* $Id: gshow.F,v 1.1.1.1 1996/02/15 17:54:25 mclareni Exp $
*
* $Log: gshow.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:25  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      SUBROUTINE GSHOW (LUN)
C
C CERN PROGLIB# Z001    GSHOW           .VERSION KERNDGE  1.01  860217
C ORIG. 03/02/86  JZ
C
C--   STORE PAM TITLE INTO /SLATE/ FOR PRINTING BY GSHOWF
C
      COMMON /SLATE/ CH(40)
C
      CALL UBLOW (
     + 40H CPP_TITLE                 
     +,  CH,40)
      CALL GSHOWF (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_GSHOW
#undef CERNLIB_TCGEN_GSHOW
#endif
#include "kerndge/TITLE.kerndge"
