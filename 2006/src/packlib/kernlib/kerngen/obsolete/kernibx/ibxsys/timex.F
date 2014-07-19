*
* $Id: timex.F,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
*
* $Log: timex.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE TIMEX (T)
C
C CERN PROGLIB# Z007    TIMEX etc       .VERSION KERNIBX  1.02  900801
C ORIG. 20/07/90  RH
C
      LOGICAL      FIRST
      REAL*8       TEND, BASE, LAST, NOW, DDB
      SAVE         TEND, BASE, LAST, TLORG, FIRST
      PARAMETER    (FACT = 1.E-6)

      DATA  FIRST  / .TRUE. /
      DATA  LAST   / 0.0 /

      IF (FIRST)  THEN
          T = 0.
          RETURN
        ENDIF

      CALL CPUTIME (NOW,IRC)
      DDB  = NOW - BASE
      T    = DDB * FACT
      RETURN

C----              TIMEL

      ENTRY TIMEL (T)

      IF (FIRST)  THEN
          T = 999.
          RETURN
        ENDIF

      CALL CPUTIME (NOW,IRC)
      DDB  = TEND - NOW
      T    = DDB * FACT
      RETURN

C----              TIMEST

      ENTRY TIMEST (TLIM)

      IF (.NOT.FIRST)             RETURN
      TLORG = TLIM
      CALL CPUTIME (NOW,IRC)
      BASE = NOW
      TEND = BASE + TLORG
      FIRST = .FALSE.
      RETURN

C----              TIMED

      ENTRY TIMED (T)

      CALL CPUTIME (NOW,IRC)
      DDB  = NOW - LAST
      LAST = NOW
      T    = DDB * FACT
      RETURN
      END
#ifdef CERNLIB_CCGEN_TIMEL
#undef CERNLIB_CCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
#ifdef CERNLIB_TCGEN_TIMEST
#undef CERNLIB_TCGEN_TIMEST
#endif
