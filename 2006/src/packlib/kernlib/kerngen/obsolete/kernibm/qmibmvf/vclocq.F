*
* $Id: vclocq.F,v 1.1.1.1 1996/02/15 17:53:05 mclareni Exp $
*
* $Log: vclocq.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:05  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
C@PROCESS DIR('*VDIR:') VECTOR OPT(3) NOSDUMP
      SUBROUTINE VCLOCQ
C CERN PROGLIB# Z004    VCLOC           .VERSION KERNIBM  2.30  900508
C
C Initialise vector timings
C
C Should be invoked before the first CALL to VCLOC
C
C Author: M.Roethlisberger/IBM
C Date  : 12-03-90
C --------------------------------------------------------------------
      PARAMETER (LEN  = 2)
      DIMENSION A(LEN)
C*VDIR: PREFER VECTOR
      DO I = 1,LEN
        A(I) = 0.
      ENDDO
      RETURN
      END
