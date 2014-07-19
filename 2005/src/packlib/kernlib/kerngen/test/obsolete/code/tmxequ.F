*
* $Id: tmxequ.F,v 1.1.1.1 1996/02/15 17:55:02 mclareni Exp $
*
* $Log: tmxequ.F,v $
* Revision 1.1.1.1  1996/02/15 17:55:02  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "_kerngent/pilot.h"
      SUBROUTINE TMXEQU

#include "mkcde.inc"

      DIMENSION    AC(9),BC(6),CHECK(6)
      INTEGER TINF(4),TINF2
      DATA  AC/ 52.,-36., 28., 35., 72., 41., 27., 14.,-16./
     +,     BC/ 49.,-98.,-62.,124.,-76.,152./
      DATA  CHECK/ -1.009187509E+00,  2.018375018E+00, -1.405379155E+00
     +,             2.810758311E+00,  1.817289317E+00, -3.634578635E+00/
      DATA   TINF /1000, 4HMXEQ, 0, 4H332 /, TINF2 /4HXEQ1/

      CALL NEWGUY ('MXEQU-MXEQU1.','TMXEQU  ')
      ZERLEV=ZEROV(3)
      CALL UCOPY (AC,A,9)
      CALL UCOPY (BC,B,9)
      CALL MXEQU (A,B,3,2)
      CALL MVERIF (1,B,CHECK,6)

      CALL UCOPY (BC,B,6)
      CALL MXEQU1 (A,B,3,2)
      CALL MVERIF  (2,B,CHECK,6)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
      CALL UCOPY (AC,A,9)
      CALL UCOPY (BC,B,6)
   80 CALL MXEQU (A,B,3,2)

      CALL TIMING (TINF)
      TINF(2)=TINF2
      CALL TIMED (TIMERD)

      DO 81 J=1,NTIMES
      CALL UCOPY (BC,B,6)
   81 CALL MXEQU1 (A,B,3,2)

      CALL TIMING (TINF)
      RETURN
      END
