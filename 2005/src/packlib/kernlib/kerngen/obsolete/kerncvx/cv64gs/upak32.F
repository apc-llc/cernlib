*
* $Id: upak32.F,v 1.1.1.1 1996/02/15 17:52:20 mclareni Exp $
*
* $Log: upak32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:20  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE UPAK32 (MTAK,MPUT,NW)
C
C CERN PROGLIB#         UPAK32          .VERSION KERNCVX  1.00  890315
C ORIG. 13/03/89, JZ
C
C     CONVEX special for ZEBRA FZIN : unpack NW/2 words from
C     MTAK to NW words in MPUT, 32 bits right justified

      DIMENSION    MTAK(*), MPUT(*)
      PARAMETER   (MASK32 = 'FFFFFFFF'X)


      NLOOP = NW/2
      JPUT  = 0

      DO 29  JTAK=1,NLOOP
      MWD = MTAK(JTAK)
      MPUT(JPUT+1) = ISHFT(MWD,-32)
      MPUT(JPUT+2) = MWD .AND. MASK32
   29 JPUT = JPUT + 2
      RETURN
      END
#ifdef CERNLIB_TCGEN_UPAK32
#undef CERNLIB_TCGEN_UPAK32
#endif
