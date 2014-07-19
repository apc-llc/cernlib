*
* $Id: uh1toc.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: uh1toc.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE UH1TOC (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UH1TOC          .VERSION KERNALT  1.00  880212
C ORIG. 10/02/88  JZ
C
      DIMENSION    MS(99), MT(99), NCHP(9)
 
      PARAMETER    (MASK1  = 'FF000000'X)
      PARAMETER    (MASK2  = 'FFFF0000'X)
      PARAMETER    (MASK3  = 'FFFFFF00'X)
 
 
      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 NWT    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JS     = 0
      IF (NWT.EQ.0)          GO TO 26
 
C--                Pack the initial complete words
 
      DO 24 JT=1,NWT
      MT(JT) = IOR (IOR (IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)), -8))
     +,           ishft (IAND(MASK1,MS(JS+3)),-16))
     +,           ishft            (MS(JS+4) ,-24))
   24 JS = JS + 4
 
      IF (NTRAIL.EQ.0)       RETURN
 
C--                Pack the trailing word
 
   26 GO TO ( 28, 27), NTRAIL
 
      MT(NWT+1) = IOR (IOR (IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)), -8))
     +,           ishft (IAND(MASK1,MS(JS+3)),-16))
     +,           IAND  (NOT(MASK3),MT(NWT+1)))
      RETURN
 
   27 MT(NWT+1) = IOR (IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           ishft (IAND(MASK1,MS(JS+2)), -8))
     +,           IAND  (NOT(MASK2),MT(NWT+1)))
      RETURN
 
   28 MT(NWT+1) = IOR (
     +                   IAND(MASK1,MS(JS+1))
     +,           IAND  (NOT(MASK1),MT(NWT+1)))
   29 RETURN
 
   91 CALL ABEND
      END
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
