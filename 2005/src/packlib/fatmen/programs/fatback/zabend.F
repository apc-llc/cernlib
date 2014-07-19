*
* $Id: zabend.F,v 1.1.1.1 1996/03/07 15:17:37 mclareni Exp $
*
* $Log: zabend.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:37  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE ZABEND
      CHARACTER*255 CHDIR
 
      PRINT 9001
9001  FORMAT(' FATBACK. ZABEND routine reached - ZPOSTM cancelled')
      CALL RZCDIR(CHDIR,'R')
      LDIR = LENOCC(CHDIR)
 
      PRINT 9002,CHDIR(1:LDIR)
9002  FORMAT(' FATBACK. current directory is ',A)
 
#if defined(CERNLIB_VAXVMS)
      CALL SYS$EXIT(%VAL(1736938))
#endif
#if defined(CERNLIB_UNIX)
      CALL FAEXIT(16)
#endif
#if defined(CERNLIB_IBM)
      STOP 16
#endif
      END
