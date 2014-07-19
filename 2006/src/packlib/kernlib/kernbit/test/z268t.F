*
* $Id: z268t.F,v 1.2 1996/09/20 14:37:04 cernlib Exp $
*
* $Log: z268t.F,v $
* Revision 1.2  1996/09/20 14:37:04  cernlib
* mods for Linux
*
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_OS9))
      SUBROUTINE Z268T
      CHARACTER*128   COMMAND
      CHARACTER*128  BUFFER
      INTEGER         LEN
      INTEGER         NL
      INTEGER         NC
      INTEGER         RC
      INTEGER         OV

      CALL KBHDR('Z268 SYSTEMS')
#ifndef CERNLIB_LINUX
      COMMAND='ls 2>&1 \0'
#else
      COMMAND='ls 2>&1 '
#endif
      LEN = 128
      CALL SYSTEMS(COMMAND,BUFFER,LEN,NL,NC,RC,OV)
      WRITE (*, *) "Number of chars: ",NC
      WRITE (*, *) "Number of lines: ",NL
      WRITE (*, *) "returned code:",RC
      IF (OV.GT.0) THEN
              WRITE (*, *) "buffer is overflowed"
      ENDIF
      WRITE (6, *) "Buffer content :"
      WRITE (6, *) '>'//BUFFER(1:NC)//'<'
      RETURN
      END
#endif
