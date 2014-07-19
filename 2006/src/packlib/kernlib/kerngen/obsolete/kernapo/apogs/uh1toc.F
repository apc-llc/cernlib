*
* $Id: uh1toc.F,v 1.1.1.1 1996/02/15 17:51:06 mclareni Exp $
*
* $Log: uh1toc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:06  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if !defined(CERNLIB_QMAPO10)
      SUBROUTINE UH1TOC (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UH1TOC          .VERSION KERNAPO  1.11  880222
C ORIG. 10/02/88  JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)

      PARAMETER    (MASK1  = 16#FF000000)
      PARAMETER    (MASK2  = 16#FFFF0000)
      PARAMETER    (MASK3  = 16#FFFFFF00)


      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 NWT    = RSHFT (NCH,2)
      NTRAIL = AND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 26

C--                Pack the initial complete words

      DO 24 JT=1,NWT
      MT(JT) = OR (OR (OR (
     +                   AND(MASK1,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)), 8))
     +,           RSHFT (AND(MASK1,MS(JS+3)),16))
     +,           RSHFT           (MS(JS+4) ,24))
   24 JS = JS + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   26 GO TO ( 28, 27), NTRAIL

      MT(NWT+1) = OR (OR (OR (
     +                   AND(MASK1,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)), 8))
     +,           RSHFT (AND(MASK1,MS(JS+3)),16))
     +,           AND  (NOT(MASK3),MT(NWT+1)))
      RETURN

   27 MT(NWT+1) = OR (OR (
     +                   AND(MASK1,MS(JS+1))
     +,           RSHFT (AND(MASK1,MS(JS+2)), 8))
     +,           AND  (NOT(MASK2),MT(NWT+1)))
      RETURN

   28 MT(NWT+1) = OR (
     +                   AND(MASK1,MS(JS+1))
     +,           AND  (NOT(MASK1),MT(NWT+1)))
   29 RETURN

   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
#endif
