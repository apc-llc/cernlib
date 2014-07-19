*
* $Id: fmlogl.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fmlogl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLOGL (LOGL)
*
************************************************************************
*
*        SUBR. FMLOGL (LOGL)
*
*   Sets the logging level inside FAZ
*
*   Arguments :
*
*     LOGL     Logging level
*
*   Called by user
*
************************************************************************
*
#include "fatmen/fatbank.inc"
      SAVE NENTRY
      DATA NENTRY/0/
 
      IF(LOGL.EQ.999.AND.NENTRY.NE.0) RETURN
      IF(NENTRY.EQ.0) NENTRY = 1
      IF(LOGL.EQ.999) THEN
         IDEBFA = 0
      ELSE
         IDEBFA = LOGL
         IF(IDEBFA.GE.0) PRINT *,'FMLOGL. setting log level to ',IDEBFA
#if defined(CERNLIB_CSPACK)
         CALL XZLOGL(IDEBFA)
#endif
      ENDIF
*                                                             END FMLOGL
      END
 
