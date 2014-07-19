*
* $Id: zabend.F,v 1.2 2000/01/04 17:13:55 mclareni Exp $
*
* $Log: zabend.F,v $
* Revision 1.2  2000/01/04 17:13:55  mclareni
* Correct includes
*
* Revision 1.1.1.1  1996/02/28 16:23:33  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE ZABEND
      CHARACTER*255 CHDIR
 
      PRINT 9001
9001  FORMAT(' CDBACKUP. ZABEND routine reached - ZPOSTM cancelled')
      CALL RZCDIR(CHDIR,'R')
      LDIR = LENOCC(CHDIR)
 
      PRINT 9002,CHDIR(1:LDIR)
9002  FORMAT(' CDBACKUP. current directory is ',A)
 
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
