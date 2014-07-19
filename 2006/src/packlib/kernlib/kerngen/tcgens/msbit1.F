*
* $Id: msbit1.F,v 1.2 2006/09/15 09:35:22 mclareni Exp $
*
* $Log: msbit1.F,v $
* Revision 1.2  2006/09/15 09:35:22  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:19  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMSUN)
#include "sungs/msbit1.F"
#else
      FUNCTION MSBIT1 (IZW,IZP)
C
C CERN PROGLIB# M421    MSBIT1          .VERSION KERNFOR  4.23  891215
C ORIG. 13/03/89  JZ
C
C     This non-ANSI code is a default which may be slow, if so
C     it should be replaced by a machine-specific fast routine

#if !defined(CERNLIB_QISASTD) && !defined(CERNLIB_GFORTRAN)
#include "kerngen/q_andor.inc"
#include "kerngen/q_shift.inc"
* Ignoring t=pass
#endif

#if !defined(CERNLIB_QISASTD) && !defined(CERNLIB_GFORTRAN)
      MSBIT1 = IOR (IZW, ISHFTL(1,IZP-1))
#endif
#if defined(CERNLIB_QISASTD) || defined(CERNLIB_GFORTRAN)
      MSBIT1 = IOR (IZW, ISHFT (1,IZP-1))
#endif
      RETURN
      END
#endif
