*
* $Id: ctoascib.F,v 1.1.1.1 1996/03/08 15:21:52 mclareni Exp $
*
* $Log: ctoascib.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:52  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_IBM)
      SUBROUTINE CTOASC(IARR,NW)
C.+++++++ IBM VERSION +++++++
C.   IBM INTERNAL ( = EBCDIC ) TO ASCII CODE CONVERSION
C.   INPUT
C.   NW      NO. OF CHARACTERS TO CONVERT
C.   I/O
C.   IARR    CONTAINS CHARACTERS UNPACKED, RIGHT ADJ., ZERO FILLED
C.           BEFORE AND AFTER CONVERSION IN PLACE.
      DIMENSION IARR( 1)
#include "epio/asciiibm.inc"
      LOGICAL*1 ITASC1(256),LK(4)
      EQUIVALENCE (K,LK(1))
      EQUIVALENCE (ITASC1(1),ITASC4(1))
      DATA IFI /0/, K/0/
      IF(IFI.NE.0)  GOTO 1
C--- INITIALIZE
      CALL CFRASC(IFI,0)
      IFI=1
    1 CONTINUE
      IF(NW.LE.0)  GOTO 77777
      DO 11  J=1,NW
      I= MIN0 (256,MAX0(1,IARR(J)))
      LK(4)= ITASC1(I)
   11 IARR(J)= K
77777 RETURN
      END
#endif
