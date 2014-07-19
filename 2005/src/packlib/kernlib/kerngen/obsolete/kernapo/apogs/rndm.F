*
* $Id: rndm.F,v 1.1.1.1 1996/02/15 17:51:05 mclareni Exp $
*
* $Log: rndm.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:05  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION RNDM (ISEED)
C
C CERN PROGLIB# V104    RNDM            .VERSION KERNAPO  1.19  891207
C ORIG. 22-mar-88 JZ from IBM version of G.Marsaglia et al., Montreal
C CORR. 13-july-88, JZ, avoid exact zero
C CORR. 13-oct-88, JZ, correct multiplier
C CORR. 11-nov-89, JZ, correct result from IRNDM
C
C-    Uniform Random Number Generator,
C-    giving the same sequence as the IBM and VAX version

      EQUIVALENCE (AMAN,MANT)
      SAVE  MCGN
      DATA  MCGN  /12345/

      MCGN = MCGN * 69069
      MANT = RSHFT (MCGN,8)
      IF (MANT.EQ.0)         GO TO 14
      AMAN = MANT
C-    AMAN in the range 1 to 2**24-1
      MANT = MANT - 16#0C000000
C-    multiply by 2.**(-24)
      RNDM = AMAN
      RETURN

C--   for zero set RNDM = 2.**(-25)
   14 MANT = 16#33000000
      RNDM = AMAN
      RETURN

C--       Integer random number
      ENTRY IRNDM (ISEED)
      MCGN = MCGN * 69069
      IRNDM = RSHFT (MCGN,1)
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
#ifdef CERNLIB_TCGEN_IRNDM
#undef CERNLIB_TCGEN_IRNDM
#endif
