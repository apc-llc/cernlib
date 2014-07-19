*
* $Id: iacwk.F,v 1.2 1996/06/05 10:20:28 cernlib Exp $
*
* $Log: iacwk.F,v $
* Revision 1.2  1996/06/05 10:20:28  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.09/00 02/11/89  16.25.36  by  O.Couet
*-- Author :
      SUBROUTINE IACWK(IWKID)
*.===========>
*.
*.   This routine actives a workstation .
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
      IF(IWKID.EQ.0)THEN
         CALL IGERR('Workstation identifier equal 0','IACWK')
         RETURN
      ENDIF
      DO 10 I=1,INOPWK
         IF(ILOPWK(I).EQ.IWKID)THEN
            ACWKFL(I)=.TRUE.
            GOTO 20
         ENDIF
 10   CONTINUE
      CALL IGERR('Workstation is not open','IACWK')
      RETURN
 20   CONTINUE
#if defined(CERNLIB_PSCRIPT)
*
      IF(IGIWTY(IWKID).LT.0)THEN
         PFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_GKS)
*
      CALL GACWK(IWKID)
#endif
*
      END
#endif
