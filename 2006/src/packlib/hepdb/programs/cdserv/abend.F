*
* $Id: abend.F,v 1.2 1996/03/12 13:07:49 cernlib Exp $
*
* $Log: abend.F,v $
* Revision 1.2  1996/03/12 13:07:49  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE ABEND
      INTEGER SYSTEMF
 
      PRINT *,'ABEND. call for CDSERV'
 
#if defined(CERNLIB_UNIX)
      PRINT *,'ABEND. calling CDABEND script'
      IC = SYSTEMF('cdabend')
#endif
#if defined(CERNLIB_VAXVMS)
      PRINT *,'ABEND. calling CDABEND command file'
      IC = SYSTEMF('@CDABEND')
#endif
#if defined(CERNLIB_IBMVM)
      PRINT *,'ABEND. calling CDABEND exec'
      IC = SYSTEMF('EXEC CDABEND')
#endif
 
      END
