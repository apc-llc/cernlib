*
* $Id: msbyt.F,v 1.2 2006/09/15 09:35:22 mclareni Exp $
*
* $Log: msbyt.F,v $
* Revision 1.2  2006/09/15 09:35:22  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:13  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMSUN)
#include "sungs/msbyt.F"
#else
      FUNCTION MSBYT (MZ,IZW,IZP,NZB)
C
C CERN PROGLIB# M421    MSBYT           .VERSION KERNFOR  4.23  891215
C ORIG. 13/03/89  JZ
C
C     This non-ANSI code is a default which may be slow, if so
C     it should be replaced by a machine-specific fast routine
#include "kerngen/wordsize.inc"
#include "kerngen/iall1s.inc"
#if !defined(CERNLIB_QISASTD) && !defined(CERNLIB_GFORTRAN)
#include "kerngen/q_andor.inc"
#include "kerngen/q_shift.inc"
* Ignoring t=pass
#endif

#if !defined(CERNLIB_QISASTD) && !defined(CERNLIB_GFORTRAN)
      MSBYT = IOR (
     +     IAND(IZW, NOT(ISHFTL(ISHFTR(IALL11, NBITPW-NZB),IZP-1)))
     +,    ISHFTR(ISHFTL(MZ,NBITPW-NZB), NBITPW+1-IZP-NZB) )
#endif
#if defined(CERNLIB_QISASTD) || defined(CERNLIB_GFORTRAN)
      MSBYT = IOR (
     +     IAND(IZW, NOT(ISHFT (ISHFT (IALL11,-NBITPW+NZB),IZP-1)))
     +,    ISHFT (ISHFT (MZ,NBITPW-NZB),-NBITPW-1+IZP+NZB) )
#endif
      RETURN
      END
#endif
