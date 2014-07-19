*
* $Id: kastop.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kastop.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KASTOP
*
*.....TERMINATE KAPACK PROCESSING
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax070.inc"
#include "kapack/kax0b0.inc"
*
*-----------------------------------------------------------------------
*
*.....DELETE ALL LOCKS
* . . (TO AVOID RECURSION VIA KAXEND DO NOT CALL LOWER LEVEL ROUTINES)
      DO  1  L = 1, LLOCKQ
         IF ( LOCKN(L) .NE. 0 )  THEN
            LUN = LOCKU(L)
            READ(LUN,REC=LOCKI(L)) (IB(I),I=1,LBKS(LUN))
            IF ( IB(9) .EQ. JOBID )  THEN
               IB(9) = 0
               WRITE(LUN,REC=LOCKI(L)) (IB(I),I=1,LBKS(LUN))
            ENDIF
            LOCKN(L) = 0
         ENDIF
 1    CONTINUE
*
      END
