*
* $Id: iwd16stf.F,v 1.1.1.1 1996/03/08 15:21:48 mclareni Exp $
*
* $Log: iwd16stf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:48  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if (defined(CERNLIB_STF77))&&(!defined(CERNLIB_STF77VX))
      FUNCTION IWD16(SOURCE,N1)
C
C     FUNCTION RETURNS THE 16 BIT WORD NO. N1  OF STRING  SOURCE.
C
C  I.MCLAREN 14/06/90
C
      INTEGER SOURCE(*)
C
#include "epio/wordsize.inc"
C
C                  SOURCE word
      IWSRC =  (N1-1)/N16PW
      I16SRC = N1-IWSRC*N16PW
      IWSRC  = IWSRC+1
      NBITR  = (N16PW-I16SRC)*16
C
C                  Extract 16-bit word from source
      IWD16 = IAND(ISHFT(SOURCE(IWSRC),-NBITR),65535)
C
      RETURN
      END
#endif
