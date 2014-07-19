*
* $Id: intrac.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: intrac.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if defined(CERNLIB_QF_APO)
      LOGICAL FUNCTION INTRAC (DUMMY)
C
C CERN PROGLIB# Z044    INTRAC          .VERSION KERNAPO  1.12  880323
C ORIG. 23/03/88  JZ
C
C--   User is on-line if stream 0 behaves like a terminal

      IOFLCN = IOS_$INQ_CONN_FLAGS (0,ISTAT)
      IF (AND(IOFLCN,5).EQ.0)  THEN
          INTRAC = .FALSE.
        ELSE
          INTRAC = .TRUE.
        ENDIF
      RETURN
      END
#ifdef CERNLIB_CCGEN_INTRAC
#undef CERNLIB_CCGEN_INTRAC
#endif
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
#endif
