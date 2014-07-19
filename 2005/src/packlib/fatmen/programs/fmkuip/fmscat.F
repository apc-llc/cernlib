*
* $Id: fmscat.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmscat.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSCAT
*
*     Display FATMEN catalogues that are available
*
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      PRINT *
      PRINT *,'FMSCAT. list of FATMEN catalogues...'
      PRINT *
#endif
#if defined(CERNLIB_UNIX)
      CALL SYSTEMF('printenv | grep FM')
#endif
#if defined(CERNLIB_VAXVMS)
      CALL SYSTEMF('SHOW SYMBOL FM*')
#endif
#if (!defined(CERNLIB_UNIX))&&(!defined(CERNLIB_VAXVMS))
      PRINT *,'FMSCAT. this command only works on ',
     +   'VMS or Unix systems'
#endif
      END
