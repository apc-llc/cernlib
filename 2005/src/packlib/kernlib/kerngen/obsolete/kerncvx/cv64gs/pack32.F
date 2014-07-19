*
* $Id: pack32.F,v 1.1.1.1 1996/02/15 17:52:19 mclareni Exp $
*
* $Log: pack32.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:19  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PACK32 (MTAK,MPUT,NW)
C
C CERN PROGLIB#         PACK32          .VERSION KERNCVX  1.00  890315
C ORIG. 13/03/89, JZ
C
C     CONVEX special for ZEBRA FZOUT : pack NW words from
C     MTAK to NW/2 words in MPUT, taking the 32 right-most bits
C     MPUT may be MTAK, ie. pack in situ

      DIMENSION    MTAK(*), MPUT(*)
      PARAMETER   (MASK32 = 'FFFFFFFF'X)


      NLOOP = NW/2
      JTAK  = 0

      DO 29  JPUT=1,NLOOP
      MWD =    ISHFT (MTAK(JTAK+1), 32)
      MWD = MWD .OR. (MTAK(JTAK+2) .AND. MASK32)
      MPUT(JPUT) = MWD
   29 JTAK = JTAK + 2
      RETURN
      END
#ifdef CERNLIB_TCGEN_PACK32
#undef CERNLIB_TCGEN_PACK32
#endif
