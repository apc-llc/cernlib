*
* $Id: gshow.F,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: gshow.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE GSHOW (LUN)
C
C CERN PROGLIB# Z001    GSHOW           .VERSION KERNNOR  1.07  810629
C
C
C--   STORE PAM TITLE INTO /SLATE/ FOR PRINTING BY GSHOWF
C
      COMMON /SLATE/ CH(40)
C
      CALL UBLOW                                                       (
     + 40H CPP_TITLE                 
     +,  CH,40)
      CALL GSHOWF (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_GSHOW
#undef CERNLIB_TCGEN_GSHOW
#endif
#include "kernnor/TITLE.kernnor"
