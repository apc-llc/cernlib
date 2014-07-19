*
* $Id: iandetc.F,v 1.1.1.1 1996/02/15 17:51:01 mclareni Exp $
*
* $Log: iandetc.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:01  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      FUNCTION IAND (I,J)
C
C CERN PROGLIB# M441    IANDETC         .VERSION KERNAPO  1.10  870524
C ORIG. 01/10/85  T.LINDELOF
C
C     APOLLO versions of functions  IAND, IOR, IEOR

      IAND = AND(I,J)
      END
      FUNCTION IOR (I,J)

      IOR = OR(I,J)
      END
      FUNCTION IEOR (I,J)

      IEOR = XOR(I,J)
      END
