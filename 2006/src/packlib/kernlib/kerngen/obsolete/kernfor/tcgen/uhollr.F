*
* $Id: uhollr.F,v 1.1.1.1 1996/02/15 17:54:17 mclareni Exp $
*
* $Log: uhollr.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:17  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if !defined(CERNLIB_TCGEN)
      SUBROUTINE UHOLLR (VECT,NC,WORD)
C
C CERN PROGLIB# M415    UHOLLR          .VERSION KERNFOR  4.18  880425
C ORIG. 01/01/65
C
      DIMENSION VECT(9),WORD(9)
C
#include "kerngen/wordsize.inc"
C
      NW=(NC-1)/NCHAPW+1
      CALL UCOPY (WORD,VECT,NW)
      RETURN
      END
#endif
