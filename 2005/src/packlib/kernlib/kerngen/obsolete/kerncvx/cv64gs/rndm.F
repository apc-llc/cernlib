*
* $Id: rndm.F,v 1.1.1.1 1996/02/15 17:52:19 mclareni Exp $
*
* $Log: rndm.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:19  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION RNDM (ISEED)
C
C CERN PROGLIB# V104    RNDM            .VERSION KERNCVX  1.06  891207
C ORIG. 22-MAR-88 from IBM version of G.Marsaglia et al., Montreal
C
C     !!!   Version valid only for CONVEX with -p8   !!!
C
C-    Uniform Random Number Generator,

      EQUIVALENCE (AMAN,MANT)
      PARAMETER    (MSK1 = '0350000000000000'X)
      PARAMETER    (MSK2 = '3CB0000000000000'X)
      SAVE  MCGN
      DATA  MCGN  / '0000358FE5D9FF21'X /


      MCGN = MCGN * 69069
      MANT = ISHFT (MCGN,-11)
      IF (MANT.EQ.0)         GO TO 14
      AMAN = MANT
C-    AMAN in the range 1 to 2**53-1
      MANT = MANT - MSK1
C-    multiply by 2.**(-53)
      RNDM = AMAN
      RETURN

C--   for zero set RNDM = 2.**(-54)
   14 MANT = MSK2
      RNDM = AMAN
      RETURN

C--       Integer random number
      ENTRY IRNDM (ISEED)
      MCGN  = MCGN * 69069
      IRNDM = ISHFT (MCGN,-1)
      RETURN

C--       Set the seed
      ENTRY RDMIN (ISEED)
      MCGN = ISEED
      RETURN

C--       Get the seed
      ENTRY RDMOUT (ISEED)
      ISEED = MCGN
      END
#ifdef CERNLIB_TCGEN_RNDM
#undef CERNLIB_TCGEN_RNDM
#endif
