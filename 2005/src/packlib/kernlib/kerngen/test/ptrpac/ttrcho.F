*
* $Id: ttrcho.F,v 1.1.1.1 1996/02/15 17:55:02 mclareni Exp $
*
* $Log: ttrcho.F,v $
* Revision 1.1.1.1  1996/02/15 17:55:02  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TTRCHO
#include "kerngent/mkcde.inc"
      DIMENSION AC(10), BC(10), DC(10), PC(10), EC(10)
      INTEGER TINF(4), TINFT2, TINFM2, TINFM4
      DATA AC/1.,0.,0.,2.,0.,13.,4.,0.,23.,77./,
     1     BC/21.,26.,34.,0.,0.,0.,24.,30.,0.,36./,
     2     DC/1.,2.,3.,0.,0.,0.,4.,5.,0.,6./,
     3     PC/1.,0.,0.,2.,0.,3.,4.,0.,5.,6./,
     4     EC/1.,2.,13.,0.,0.,0.,4.,23.,0.,77./
      DATA TINF/1000,4HCHLU,1,4H4*4 /
      DATA TINFT2/4HCHUL/,
     +     TINFM2/4HSMUL/, TINFM4/4H4*4 /,
     +     TINFL2/4HSMLU/
C-----------------------------------------------------------------------

      CALL NEWGUY ('TRCHLU-TRCHUL.','TTRCHO  ')
      ZERLEV = ZEROV(3)

      CALL  TRCHLU (AC,A,4)
      CALL MVERIF (11,A,PC,10)

      CALL UCOPY (AC,A,10)
      CALL  TRCHLU (A ,A,4)
      CALL MVERIF (12,A,PC,10)

      CALL  TRCHUL (BC,A,4)
      CALL MVERIF (21,A,DC,10)

      CALL UCOPY (BC,A,10)
      CALL  TRCHUL (A ,A,4)
      CALL MVERIF (22,A,DC,10)
C - - - - - - - - - - - - - - - - - - - -- -  - -- - - - - - - - - - - -

      IF (ITIMES .EQ. 0)   GO TO 120
      CALL TIMED (TIMERD)
      DO 50  J=1,1000
   50 CALL  TRCHLU (AC,A,4)
      CALL TIMING (TINF)

      TINF(2) = TINFT2
      CALL TIMED (TIMERD)
      DO 100 J=1,1000
  100 CALL  TRCHUL (BC,A,4)
      CALL TIMING (TINF)

C-----------------------------------------------------------------------
  120 CALL NEWGUY ('TRSMUL-TRSMLU.','TTRCHO  ')
      ZERLEV = ZEROV(2)

      CALL TRSMUL(DC,A,4)
      CALL MVERIF (11,A,BC,10)

      CALL UCOPY (DC,A,10)
      CALL TRSMUL (A ,A,4)
      CALL MVERIF (12,A,BC,10)

      CALL TRSMLU(DC,A,4)
      CALL MVERIF(22,A,EC,10)

      CALL UCOPY (DC,A,10)
      CALL TRSMLU(A,A,4)
      CALL MVERIF(21,A,EC,10)
C - - - - - - - - - - - - - - - - - - - -- -  - -- - - - - - - - - - - -

      IF (ITIMES .EQ. 0)   RETURN
      TINF(2) = TINFM2
      TINF(4) = TINFM4
      CALL TIMED (TIMERD)
      DO 150 J=1,1000
  150 CALL TRSMUL (DC,A,4)
      CALL TIMING (TINF)

      TINF(2)=TINFL2
      CALL TIMED (TIMERD)
      DO 160 J=1,1000
  160 CALL TRSMLU(DC,A,4)
      CALL TIMING (TINF)

      RETURN
      END
