*
* $Id: isfais.F,v 1.3 1996/12/09 13:47:28 couet Exp $
*
* $Log: isfais.F,v $
* Revision 1.3  1996/12/09 13:47:28  couet
* - protection added against IWTYP=0
*
* Revision 1.2  1996/09/25 14:58:56  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 08/05/95  11.49.38  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISFAIS(INTS)
      ENTRY      GSFAIS(INTS)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
      IF(INTS.EQ.IFAIS)RETURN
      IF((INTS.LT.0).OR.(INTS.GT.3))THEN
         CALL IGERR('Fill area interior style must be > -1 and < 4'
     +,             'ISFAIS')
         RETURN
      ENDIF
      IFAIS=INTS
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFAICO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_FALCO)
         IF(IWTYPE.EQ.7878.OR.IWTYPE.EQ.7879)RETURN
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
         IF(IFAIS.EQ.1)CALL SETPAT(0)
#endif
#if (defined(CERNLIB_X11)||defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_NDP))
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IXSETFS(IFAIS,ABS(IFASI))
   10    CONTINUE
#endif
#if defined(CERNLIB_MACMPW)
         CALL IMSETFS(IFAIS,ABS(IFASI))
#endif
#if defined(CERNLIB_PHIGS)
         CALL PSIS(IFAIS)
#endif
      ENDIF
      END
