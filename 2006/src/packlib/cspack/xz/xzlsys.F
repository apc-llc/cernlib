*
* $Id: xzlsys.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzlsys.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLSYS
*-- Author :    Jamie Shiers   17/07/91
      PRINT *,'Local operating system:'
#if defined(CERNLIB_IBMVM)
         PRINT *,'VM'
#endif
#if defined(CERNLIB_IBMMVS)
         PRINT *,'MVS'
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'VMS'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'UNIX'
#endif
      END
