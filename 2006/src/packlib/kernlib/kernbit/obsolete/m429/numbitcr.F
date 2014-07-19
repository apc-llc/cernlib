*
* $Id: numbitcr.F,v 1.1.1.1 1996/02/15 17:47:42 mclareni Exp $
*
* $Log: numbitcr.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:42  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_CRAY)
      FUNCTION NUMBIT(I)
C--      FUNCTION TO COUNT THE NUMBER OF 1-BITS IN A WORD
C--     USE THE FTN LIBRARY POPCNT FUNCTION
      NUMBIT= POPCNT(I)
      RETURN
      END
#endif
