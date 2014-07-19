*
* $Id: ischup.F,v 1.2 1996/06/05 10:20:40 cernlib Exp $
*
* $Log: ischup.F,v $
* Revision 1.2  1996/06/05 10:20:40  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/04 10/02/89  14.28.33  by  O.Couet
*-- Author :
      SUBROUTINE ISCHUP(CHUX,CHUY)
*.===========>
*.
*.   This routine sets the character up vector . This vector gives the text
*. inclination .
*.
*. _Input parameters:
*.
*.  REAL CHUX,CHUY : Character up vector coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF((CHUX.EQ.RCHUX).AND.(CHUY.EQ.RCHUY))RETURN
      RCHUX=CHUX
      RCHUY=CHUY
      IF(GFLAG)CALL GSCHUP(CHUX,CHUY)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF((RCHUX.EQ.0.).AND.(RCHUY.EQ.1.))THEN
         RANGLE=0.
      ELSEIF((RCHUX.EQ.-1.).AND.(RCHUY.EQ.0.))THEN
         RANGLE=90.
      ELSEIF((RCHUX.EQ.0.).AND.(RCHUY.EQ.-1.))THEN
         RANGLE=180.
      ELSEIF((RCHUX.EQ.1.).AND.(RCHUY.EQ.0.))THEN
         RANGLE=270.
      ELSE
         RANGLE=((ACOS(RCHUX/SQRT(RCHUX**2+RCHUY**2))*180.)
     +           /3.14159)-90.
         IF(RCHUY.LT.0.)RANGLE=180.-RANGLE
         IF(ABS(RANGLE).LE.0.01)RANGLE=0.
      ENDIF
      IF(GLFLAG)CALL IZATT(IANGCO)
#endif
*
      END
#endif
