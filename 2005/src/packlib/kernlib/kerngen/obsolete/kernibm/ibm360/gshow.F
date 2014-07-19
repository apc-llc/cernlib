*
* $Id: gshow.F,v 1.1.1.1 1996/02/15 17:53:10 mclareni Exp $
*
* $Log: gshow.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:10  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GSHOW (LUN)
C
C CERN PROGLIB# Z001    GSHOW           .VERSION KERNIBM  1.04  781218
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
#include "TITLE.kernibm"
