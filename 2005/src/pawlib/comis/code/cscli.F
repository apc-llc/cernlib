*
* $Id: cscli.F,v 1.2 1996/05/13 08:04:57 berejnoi Exp $
*
* $Log: cscli.F,v $
* Revision 1.2  1996/05/13 08:04:57  berejnoi
* Mods for SHL version on IBMRT
*
* Revision 1.1.1.1  1996/02/26 17:16:18  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 09/11/94  19.49.00  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSCLI(COMMAN)
*****************************************************
*                                                   *
*   ISSUES THE COMMAND TO CLI AND SETS THE PROGRAM  *
*   TO WAIT UNTIL THE COMPLETION OF THE COMMAND     *
*   EXECUTION.                                      *
*                                                   *
*                    EXAMPLE:                       *
*                                                   *
*           CALL CSCLI('ED TEXT.TXT')               *
*                                                   *
*****************************************************
*
      CHARACTER *(*) COMMAN
#if defined(CERNLIB_MSDOS) || defined(CERNLIB_LINUX)|| \
    defined(CERNLIB_WINNT) || defined(CERNLIB_IBMRT)
      INTEGER SYSTEMF
#endif
#if defined(CERNLIB_DECS)
      INTEGER SYSTEM
#endif
*
#if defined(CERNLIB_VAX)
      CALL LIB$SPAWN(COMMAN)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CALL VMCMS(COMMAN,IREP)
      IF(IREP.NE.0)PRINT *,COMMAN,IREP
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KMVSSH(COMMAN)
      IREP = 0
#endif
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT)
      IER = SYSTEMF(COMMAN)
#endif
#if defined(CERNLIB_LINUX) || defined(CERNLIB_IBMRT)
      CALL CUTOL(COMMAN)
      IER = SYSTEMF(COMMAN)
#endif
#if defined(CERNLIB_DECS)
      CALL CUTOL(COMMAN)
      IER = SYSTEM(COMMAN)
#endif
*
      END
