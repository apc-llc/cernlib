*
* $Id: ibits.F,v 1.2 2006/09/15 09:35:19 mclareni Exp $
*
* $Log: ibits.F,v $
* Revision 1.2  2006/09/15 09:35:19  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:49:44  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMDOS)
#include "dosgs/ibits.F"
#elif defined(CERNLIB_QMLNX) && !defined(CERNLIB_GFORTRAN)
#include "lnxgs/ibits.F"
#elif !defined(CERNLIB_QMILSTD)
      FUNCTION IBITS(I,N,L)
C
C CERN PROGLIB# M441    IBITS           .VERSION KERNFOR  4.16  870601
C ORIG. 01/03/85  M.METCALF
C
C      FORTRAN VERSION OF MIL-STD-1753 FUNCTION IBITS
C      N IS THE RIGHTMOST BIT OF THE REQUIRED FIELD
C      L IS THE LENGTH OF THE REQUIRED FIELD
C
#include "kerngen/wordsize.inc"
C
      IBITS = ISHFT(ISHFT(I, NBITPW-L-N), L-NBITPW)
      END
#endif
