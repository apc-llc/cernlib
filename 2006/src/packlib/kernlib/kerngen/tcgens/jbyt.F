*
* $Id: jbyt.F,v 1.2 2006/09/15 09:35:21 mclareni Exp $
*
* $Log: jbyt.F,v $
* Revision 1.2  2006/09/15 09:35:21  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:11  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMSUN)
#include "sungs/jbyt.F"
#else
      FUNCTION JBYT (IZW,IZP,NZB)
C
C CERN PROGLIB# M421    JBYT            .VERSION KERNFOR  4.23  891215
C ORIG. 13/03/89  JZ
C
C     This non-ANSI code is a default which may be slow, if so
C     it should be replaced by a machine-specific fast routine
#include "kerngen/wordsize.inc"
#if !defined(CERNLIB_QISASTD) && !defined(CERNLIB_GFORTRAN)
#include "kerngen/q_andor.inc"
#include "kerngen/q_shift.inc"
* Ignoring t=pass
#endif

#if !defined(CERNLIB_QISASTD) && !defined(CERNLIB_GFORTRAN)
      JBYT = ISHFTR(ISHFTL(IZW,NBITPW+1-IZP-NZB), NBITPW-NZB)
#endif
#if ((defined(CERNLIB_QISASTD))&&(!defined(CERNLIB_QMILSTD))) || defined(CERNLIB_GFORTRAN)
      JBYT = ISHFT (ISHFT (IZW,NBITPW+1-IZP-NZB),-NBITPW+NZB)
#endif
#if defined(CERNLIB_QMILSTD)
      JBYT = IBITS (IZW,IZP-1,NZB)
#endif
      RETURN
      END
#endif
