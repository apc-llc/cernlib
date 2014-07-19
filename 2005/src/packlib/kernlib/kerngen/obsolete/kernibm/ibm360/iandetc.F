*
* $Id: iandetc.F,v 1.1.1.1 1996/02/15 17:53:12 mclareni Exp $
*
* $Log: iandetc.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:12  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_QF_SIEM)
      FUNCTION IAND (I,J)
C
C CERN PROGLIB# M441    IANDETC         .VERSION KERNIBM  2.20  870601
C
C     IBM Fortran 4 versions of functions  IAND, IOR, IEOR, NOT

      IAND = LAND(I,J)
      RETURN
      END
      FUNCTION IOR (I,J)

      IOR = LOR(I,J)
      RETURN
      END
      FUNCTION IEOR (I,J)

      IEOR = LXOR(I,J)
      RETURN
      END
      FUNCTION NOT (I)

      NOT = LCOMPL(I)
      RETURN
      END
#endif
