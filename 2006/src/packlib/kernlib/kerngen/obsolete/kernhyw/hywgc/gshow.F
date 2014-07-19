*
* $Id: gshow.F,v 1.1.1.1 1996/02/15 17:52:48 mclareni Exp $
*
* $Log: gshow.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:48  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE GSHOW (LUN)
C
C CERN PROGLIB#         GSHOW           .VERSION KERNHYW  1.00  840910
C ORIG. 17/05/83 HRR
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
*      ==================================================
#include "qcardl.inc"
#include "TITLE.kernhyw"
