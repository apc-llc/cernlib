*
* $Id: pawint1.F,v 1.2 1999/05/20 07:37:17 couet Exp $
*
* $Log: pawint1.F,v $
* Revision 1.2  1999/05/20 07:37:17  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/14 16/10/95  13.16.03  by  Julian Bunn
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWINT1(PROMPT1,BATCHF,PAWLOGF)
*
*  First level PAW initialization routine.
*  It Reads the command line arguments and returns the names of the
*  batch and pawlogon files.
*
      CHARACTER*(*) PROMPT1,BATCHF,PAWLOGF
      CHARACTER*80  LOGFIL
#include "paw/pcargs.inc"
#include "paw/pcmode.inc"
#include "paw/pawfil.inc"
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      PAWLOG=PAWLO2
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
*--   Prevents Underflow report on IBM.
      CALL XUFLOW(0)
*--   Clear screen before starting
      IRC=IOSCLR()
#endif
*
*            Batch or Interactive version ?
*
      BATCHF = ' '
      CALL KGETAR(CHARGS)
      CALL KUARGS(PROMPT1,PAWLOG,BATCHF,LOGFIL,IERR)
      IF (IERR .NE. 0) STOP
*
      NOLOG=.FALSE.
      BATCH=.FALSE.
      IF (BATCHF.NE.' ') BATCH = .TRUE.
      IF (PAWLOG.EQ.' ') NOLOG = .TRUE.
      PAWLOGF=PAWLOG
*
      END
