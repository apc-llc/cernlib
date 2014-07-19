*
* $Id: incbyt.F,v 1.1.1.1 1996/02/15 17:51:02 mclareni Exp $
*
* $Log: incbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:02  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION INCBYT (INCP,IA,JTHP,NBITS)
C
C CERN PROGLIB# M423    INCBYT          .VERSION KERNAPO  1.01  831216
C ORIG. 14/12/83  JZ
C
      DIMENSION INCP(9),IA(99),JTHP(9),NBITS(3)
C
C
      INC = INCP(1)
      JTH = JTHP(1)
      NZB = NBITS(1)
      NPW = NBITS(2)
      NPB = NBITS(3)

      INCBYT = 0

      JWORD= (JTH-1)/NPW + 1
      JPOS = JTH - (JWORD-1)*NPW
      NL   = (JPOS-1)*NZB
      MASK = LSHFT (RSHFT(INT4(-1),32-NZB),NL)

      IZW  = IA(JWORD)
      NEW  = INC + RSHFT (AND(MASK,IZW),NL)
      IF (NEW.GT.NPB)        GO TO 21
   17 IA(JWORD) = OR (AND(NOT(MASK),IZW), LSHFT(NEW,NL))
      RETURN
C
   21 INCBYT = NEW - NPB
      IF (INCBYT.EQ.INC)     RETURN
      NEW = NPB
      GO TO 17
      END
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
