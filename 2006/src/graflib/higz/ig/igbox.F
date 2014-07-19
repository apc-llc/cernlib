*
* $Id: igbox.F,v 1.2 1996/09/25 14:57:17 couet Exp $
*
* $Log: igbox.F,v $
* Revision 1.2  1996/09/25 14:57:17  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/05 17/10/95  18.11.00  by  O.Couet
*-- Author :
      SUBROUTINE IGBOX(X1,X2,Y1,Y2)
*.===========>
*.
*.   This routine draws a boxe. The boxe is drawn according to the
*.   current fill area attributes .
*.
*. _Input parameters:
*.
*.  REAL X1,Y1 : Left down corner (in WC) .
*.  REAL X2,Y2 : Right up corner (in WC) .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#if (!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))&&(!defined(CERNLIB_FALCO))
#include "higz/hiatt.inc"
      DIMENSION X(5),Y(5)
#endif
      LOGICAL ZFSAV,PFSAV
*.______________________________________
*
      ZFSAV=ZFLAG
      PFSAV=PFLAG
      ZFLAG=.FALSE.
      PFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
#if defined(CERNLIB_GL)||defined(CERNLIB_X11)||defined(CERNLIB_FALCO)||defined(CERNLIB_MACMPW)
      CALL IKBOX(X1,X2,Y1,Y2)
#endif
#if (!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))&&(!defined(CERNLIB_FALCO))&&(!defined(CERNLIB_MACMPW))
      X(1)=X1
      X(2)=X2
      X(3)=X2
      X(4)=X1
      X(5)=X1
      Y(1)=Y1
      Y(2)=Y1
      Y(3)=Y2
      Y(4)=Y2
      Y(5)=Y1
      ILNOLD=ILN
      IF(GFLAG.OR.PFLAG)THEN
         IF(IFAIS.NE.0)THEN
            CALL IFA(4,X,Y)
         ENDIF
         IF(IFAIS.EQ.0.OR.IBORD.NE.0)THEN
            CALL ISLN(1)
            CALL IPL(5,X,Y)
            CALL ISLN(ILNOLD)
         ENDIF
      ENDIF
#endif
      ZFLAG=ZFSAV
      PFLAG=PFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZBOX(X1,X2,Y1,Y2)
#endif
*
      END
