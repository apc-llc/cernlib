*
* $Id: vxinvb.F,v 1.1.1.1 1996/02/15 17:54:23 mclareni Exp $
*
* $Log: vxinvb.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE VXINVB (MVI,NW)

C CERN PROGLIB# M434    VXINVB          .VERSION KERNLNX  1.00  930829
C ORIG.  25-June-92, JZ

C-    VX invert bytes, in-place

      DIMENSION    MVI(99)
      DATA MASK1 /X'FF'/

      IF (NW .LE. 0) RETURN
      DO 29  J=1,NW
C 1-st byte
      IIN = MVI(J)
      IOT = LSHIFT(AND(IIN,MASK1), 24)
C 2-nd byte
      IIN = ISHFT(IIN, -8)
      IWORK = AND(IIN,MASK1)
      IOT   = OR(IOT, LSHIFT(IWORK,16))
C 3-d  byte
      IIN = ISHFT(IIN, -8)
      IWORK = AND(IIN,MASK1)
      IOT   = OR(IOT, LSHIFT(IWORK,8))
C last byte
      IIN = ISHFT(IIN, -8)
      IOT   = OR(IOT, IIN)

      MVI(J) = IOT
   29 CONTINUE
      END
#ifdef CERNLIB_TCGEN_VXINVB
#undef CERNLIB_TCGEN_VXINVB
#endif
