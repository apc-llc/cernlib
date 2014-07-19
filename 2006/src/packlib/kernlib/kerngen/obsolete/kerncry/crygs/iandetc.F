*
* $Id: iandetc.F,v 1.1.1.1 1996/02/15 17:52:35 mclareni Exp $
*
* $Log: iandetc.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:35  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      FUNCTION IAND (I,J)
C
C CERN PROGLIB# M441    IANDETC         .VERSION KERNCRY  1.06  870527
C
C     CRAY versions of functions  IAND, IOR, IEOR, NOT

      IAND = AND(I,J)
      RETURN
      END
      FUNCTION IOR (I,J)

      IOR = OR(I,J)
      RETURN
      END
      FUNCTION IEOR (I,J)

      IEOR = XOR(I,J)
      RETURN
      END
      FUNCTION NOT (I)

      NOT = COMPL(I)
      RETURN
      END
