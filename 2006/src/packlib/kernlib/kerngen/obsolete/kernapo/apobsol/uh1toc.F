*
* $Id: uh1toc.F,v 1.1.1.1 1996/02/15 17:51:10 mclareni Exp $
*
* $Log: uh1toc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:10  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      SUBROUTINE UH1TOC (MH,MC,NCHP)
C
C CERN PROGLIB# M409    UH1TOC          .VERSION KERNAPO  1.06  851211
C ORIG. 29/03/85  JZ
C

      DIMENSION    MH(99), NCHP(9)
      INTEGER*2    MC(99)


      NCH = NCHP(1)
      IF   (NCH)             91,39,11
   11 NW     = RSHFT (NCH,1)
      NTRAIL = RSHFT (LSHFT(NCH,31),31)
      JH     = 0
      IF (NW.EQ.0)           GO TO 31

C--                PACK THE LEADING 16-BIT WORDS

      DO 24  JC=1,NW
      MC(JC) = OR ( LSHFT (RSHFT(MH(JH+1),24), 8),
     +                     RSHFT(MH(JH+2),24) )
   24 JH = JH + 2

      IF (NTRAIL.EQ.0)       RETURN

C--                PACK THE TRAILING WORD

   31 JC = NW + 1
      MC(JC) = OR ( LSHFT (RSHFT(MH(JH+1),24), 8),
     +              RSHFT (LSHFT(MC(JC)  ,8),  8))
   39 RETURN

   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
