*
* $Id: idawk.F,v 1.2 1996/06/05 10:20:32 cernlib Exp $
*
* $Log: idawk.F,v $
* Revision 1.2  1996/06/05 10:20:32  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.13/07 06/11/91  09.21.16  by  O.Couet
*-- Author :
      SUBROUTINE IDAWK(IWKID)
*.===========>
*.
*.   This routine deactives the IWKID workstation .
*.
*. _Input parameters:
*.
*.  INTEGER IWKID : Workstation identifier .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
*.______________________________________
*
      DO 10 I=1,INOPWK
         IF(ILOPWK(I).EQ.IWKID.AND.ACWKFL(I))THEN
            ACWKFL(I)=.FALSE.
            GOTO 20
         ENDIF
 10   CONTINUE
      CALL IGERR('Workstation is not active','IDAWK')
      RETURN
 20   CONTINUE
#if defined(CERNLIB_PSCRIPT)
*
      IF(IGIWTY(IWKID).LT.0)THEN
         PFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_GKS)
*
      CALL GDAWK(IWKID)
#endif
*
      END
#endif
