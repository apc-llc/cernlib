*
* $Id: jbit.F,v 1.2 2006/09/15 09:35:21 mclareni Exp $
*
* $Log: jbit.F,v $
* Revision 1.2  2006/09/15 09:35:21  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:11  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMSUN)
#include "sungs/jbit.F"
#else
      FUNCTION JBIT (IZW,IZP)
C
C CERN PROGLIB# M421    JBIT            .VERSION KERNFOR  4.23  891215
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
      JBIT = IAND (ISHFTR(IZW, IZP-1), 1)
#endif
#if ((defined(CERNLIB_QISASTD))&&(!defined(CERNLIB_QMILSTD))) || defined(CERNLIB_GFORTRAN)
      JBIT = IAND (ISHFT (IZW,-IZP+1), 1)
#endif
#if defined(CERNLIB_QMILSTD)
      JBIT = IBITS (IZW,IZP-1,1)
#endif
      RETURN
      END
#endif
