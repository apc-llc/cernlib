*
* $Id: ctoasccd.F,v 1.1.1.1 1996/03/08 15:21:52 mclareni Exp $
*
* $Log: ctoasccd.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:52  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_CDC)
      SUBROUTINE CTOASC(IARR,NW)
C.+++++++ CDC VERSION +++++++
C.   CDC INTERNAL ( =DISPLAY ) TO ASCII CODE CONVERSION
C.   INPUT
C.   NW      NO. OF CHARACTERS TO CONVERT
C.   I/O
C.   IARR    CONTAINS CHARACTERS UNPACKED, RIGHT ADJ., ZERO FILLED
C.           BEFORE AND AFTER CONVERSION IN PLACE.
#include "epio/asciicdc.inc"
      DIMENSION IARR( 1)
      DATA IFI/0/
      IF(IFI.NE.0)  GOTO 10
C--- INITIALIZE
      CALL CFRASC(IFI,0)
      IFI=1
   10 CONTINUE
      IF(NW.LE.0)  GOTO 77777
      DO 11  I=1,NW
#if defined(CERNLIB_F4)
      K=AND(IARR(I),77B)
#endif
#if !defined(CERNLIB_F4)
      K=AND(IARR(I),O"77")
#endif
      NP=(K-1)/8
      NB=K-8*NP
#if defined(CERNLIB_F4)
   11 IARR(I)=AND(SHIFT(ITASC(NP+1),7*NB),177B)
#endif
#if !defined(CERNLIB_F4)
   11 IARR(I)=AND(SHIFT(ITASC(NP+1),7*NB),O"177")
#endif
77777 RETURN
      END
#endif
