*
* $Id: wuix.F,v 1.1.1.1 1996/02/15 17:47:54 mclareni Exp $
*
* $Log: wuix.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:54  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_UNIX)
      PROGRAM WHICH
*
* CERN PROGLIB# Z265    WHICHM          .VERSION KERNBIT  1.09  920701
* Author: Jamie Shiers
* FORTRAN version of Unix which command
*
      CHARACTER*255 CHPATH,CHPROG,CHFILE

      COMMON/SLATE/IS(40)

      CALL GETENVF('PATH',CHPATH)
      LPATH = IS(1)

      NARGS = IARGC(DUMMY)
      IF(NARGS.EQ.1) THEN
         CALL GETARG(1,CHPROG)
         CALL WHICHF(CHPATH,CHPROG,CHFILE)
         IF(IS(1).GT.0) THEN
            PRINT *,CHFILE(1:IS(1))
         ELSE
            PRINT *,'No ',CHPROG(1:LENOCC(CHPROG)),' found in ',
     +              CHPATH(1:LPATH)
         ENDIF
      ELSE
         PRINT *,'usage: which <command>'
      ENDIF

      END
#endif
