*
* $Id: getcwd.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: getcwd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if (defined(CERNLIB_IBMRT))&&(defined(CERNLIB_PLODGE))
      SUBROUTINE GETCWD_(CHPATH)
      CHARACTER*() CHPATH
      CALL GETCWD(CHPATH,L)
      END
 
#endif
