*
* $Id: isclip.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: isclip.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.21/01 11/05/94  13.30.36  by  O.Couet
*-- Author :
      SUBROUTINE ISCLIP(CLSW)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the indicator for clipping at the boundaries of
*.     the normalization transformation viewport.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JWCLIP (LVALUE)
*.
*.   Parameters:
*.   ___________
*.     LVALUE
*.      - Logical flag indicating whether or not subsequent output primitives
*.        are to be clipped to the boundaries of the clipping window on the
*.        viewplane.
*.
*.        LVALUE = .TRUE.    ---> enable window clipping
*.        LVALUE = .FALSE.   ---> disable window clipping
*.
*.      *NOTE* : If window clipping is suppressed and world coordiantes
*.                data exceed the window boundaries, the resultant display
*.                device image is unpredictable and device dependent
*.
*.      *NOTE* : DI-3000 disables clipping by default
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CLSW
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSCLI(CLSW)
#endif
      IF(CLSW.EQ.ICLIP)RETURN
      ICLIP=CLSW
      IF(GFLAG)THEN
         IF(SEGOPN) THEN
            CALL JCLOSE
            SEGOPN = .FALSE.
         ENDIF
         CALL JWCLIP(CLSW.EQ.1)
      ENDIF
*
      END
#endif
