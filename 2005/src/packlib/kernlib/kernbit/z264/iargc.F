*
* $Id: iargc.F,v 1.2 1997/02/04 17:34:08 mclareni Exp $
*
* $Log: iargc.F,v $
* Revision 1.2  1997/02/04 17:34:08  mclareni
* Merge Winnt and 97a versions
*
* Revision 1.1.1.1.2.1  1997/01/21 11:29:17  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      FUNCTION IARGC()
C
C CERN PROGLIB#    Z264 IARGC           .VERSION KERNVAX  x.xx  911130
C
C Returns number of arguments in command line
C Author: Miguel Marquina 91/11/30
C
C Mods          Date   Comments
C V.Fine      94/01/18 Windows/NT support
C M.Marquina  92/12/05 Treat quote-delimited strings as single args
C
      COMMON/Z264/IREAD,ISLEN,STRING
      CHARACTER*255 STRING
      LOGICAL LB,LQ
      IF(IREAD.EQ.0) THEN
        STRING=' '
        IFL=0
#if defined(CERNLIB_VAXVMS)
        IRC=LIB$GET_FOREIGN(STRING,,ISLEN,IFL)
#endif
#if defined(CERNLIB_WINNT)
        CALL GETCOM(STRING,ISLEN)
#endif
        IREAD=1
      ENDIF
      NW=0
      IF(ISLEN.EQ.0) GOTO 20
          LB=.TRUE.
          LQ=.FALSE.
      DO 10 N=1,ISLEN
        IF(STRING(N:N).EQ.'"') LQ=.NOT.LQ
        IF(LQ) GOTO 10
        IF(STRING(N:N).NE.' '.AND.LB) LB=.FALSE.
        IF(STRING(N:N).EQ.' '.AND..NOT.LB) THEN
          LB=.TRUE.
          NW=NW+1
        ENDIF
10    CONTINUE
      IF(STRING(N-1:N-1).NE.' ') NW=NW+1
20    CONTINUE
      IARGC=NW
#if defined(CERNLIB_WINNT)
      IARGC=IARGC-1
#endif
      RETURN
      END
