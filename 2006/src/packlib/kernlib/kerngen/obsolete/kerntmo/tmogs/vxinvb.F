*
* $Id: vxinvb.F,v 1.1.1.1 1996/02/15 17:53:50 mclareni Exp $
*
* $Log: vxinvb.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:50  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      SUBROUTINE VXINVB (MV,NW)
 
C CERN PROGLIB# M434    VXINVB          .VERSION KERNTMO  1.01  890303
C ORIG.  12-June-87, F. Carminati + J. Zoll
 
C-    VX invert bytes, in-place
C-    Fortran version, slower than the machine-language routine
 
      DIMENSION    MV(99)
      CHARACTER*1    LLD(4),    LST(4)
      EQUIVALENCE (LLD,MLD), (LST,MST)
 
      DO 29  J=1,NW
      MLD    = MV(J)
      LST(1) = LLD(4)
      LST(2) = LLD(3)
      LST(3) = LLD(2)
      LST(4) = LLD(1)
      MV(J)  = MST
   29 CONTINUE
      END
#ifdef CERNLIB_TCGEN_VXINVB
#undef CERNLIB_TCGEN_VXINVB
#endif
