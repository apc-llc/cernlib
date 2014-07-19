*
* $Id: whichf.F,v 1.1.1.1 1996/02/15 17:47:53 mclareni Exp $
*
* $Log: whichf.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:53  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE WHICHF(CHPATH,CHLOOK,CHFILE)
*
* CERN PROGLIB# Z265    WHICHF          .VERSION KERNBIT  1.09  920701
* Author: Jamie Shiers
* FORTRAN callable version of Unix which command
*
* Input:  CHPATH - character variable containing the pathname
*                 which is searched
*         CHLOOK - the program (file) to look for
* Output: CHFILE - the full name of the file
*         COMMON/SLATE/IS(40)
*         IS(1)  - length of CHFILE. 0 = no match
*
      CHARACTER*(*) CHPATH,CHLOOK,CHFILE
      COMMON/SLATE/ IS(40)
      CHARACTER*255 CHNAME,CHPROG
      LOGICAL       IEXIST

      LPATH  = LENOCC(CHPATH)
      LPROG  = LENOCC(CHLOOK)
      CHPROG = CHLOOK(1:LPROG)
*
*     Loop over all elements in path name
*
      ISTART = 1
10    CONTINUE
      ICOLON = INDEX(CHPATH(ISTART:LPATH),':')
      IF(ICOLON.NE.0) THEN
         IEND = ISTART + ICOLON - 2
      ELSE
         IEND = LPATH
      ENDIF
*
*     In case of a dot, get CWD
*
      IF(CHPATH(ISTART:IEND).EQ.'.') THEN
         CALL GETWDF(CHNAME)
         CHFILE = CHNAME(1:IS(1)) // '/'
     +            // CHPROG(1:LPROG)
      ELSE
         CHFILE = CHPATH(ISTART:IEND) // '/'
     +            // CHPROG(1:LPROG)
      ENDIF
      LFILE = LENOCC(CHFILE)
*
*     Does this file exist?
*
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
      IF(IEXIST) THEN
         IS(1) = LFILE
         RETURN
      ENDIF

      ISTART  = ISTART + ICOLON
      IF(ICOLON.NE.0) GOTO 10

      IS(1)   = 0

      END
#endif
