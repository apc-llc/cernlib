*
* $Id: isln.F,v 1.3 1998/01/28 14:34:53 couet Exp $
*
* $Log: isln.F,v $
* Revision 1.3  1998/01/28 14:34:53  couet
* - GGDM driver removed
*
* Revision 1.2  1996/09/25 14:58:57  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:57  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/07 12/01/94  10.01.55  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISLN(LTYPE)
      ENTRY      GSLN(LTYPE)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_X11)
      INTEGER DASHED(2),DOTTED(2),DSHDOT(4)
      DATA DASHED/5,5/
      DATA DOTTED/1,3/
      DATA DSHDOT/5,3,1,3/
#endif
      IF(LTYPE.EQ.ILN)RETURN
      ILN=LTYPE
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ILNCO)
#endif
      IF(GFLAG)THEN
#if defined(CERNLIB_PHIGS)
         CALL PSLN(ILN)
#endif
#if !defined(CERNLIB_BATCH)
         DO 10 IWKNB=1,INOPWK
            IF(NODRFL(IWKNB))GOTO 10
            CALL IGSG(IWKNB)
#endif
#if defined(CERNLIB_FALCO)
            IF(IWTYL(IWKNB).EQ.7878.OR.IWTYL(IWKNB).EQ.7879)THEN
               CALL IGSG(1)
               CALL IFPUT(27)
               IF(ILN.EQ.1)CALL IFPUT(IGASCI('`'))
               IF(ILN.EQ.2)CALL IFPUT(IGASCI('a'))
               IF(ILN.EQ.3)CALL IFPUT(IGASCI('c'))
               IF(ILN.EQ.4)CALL IFPUT(IGASCI('b'))
               IF(ILN.EQ.5)CALL IFPUT(IGASCI('d'))
               CALL IFPUT(0)
               GOTO 10
            ENDIF
#endif
#if defined(CERNLIB_GL)
            CALL SETLIN(ILN-1)
#endif
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_WINNT))
            IF(ILN.EQ.1)CALL IXSETLS(0,0)
            IF(ILN.EQ.2)CALL IXSETLS(2,DASHED)
            IF(ILN.EQ.3)CALL IXSETLS(2,DOTTED)
            IF(ILN.EQ.4)CALL IXSETLS(4,DSHDOT)
#endif
#if defined(CERNLIB_WINNT)
            CALL IXSETLS(-ILN,0)
#endif
#if !defined(CERNLIB_BATCH)
   10    CONTINUE
#endif
      ENDIF
      END
