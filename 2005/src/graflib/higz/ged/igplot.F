*
* $Id: igplot.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: igplot.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/00 28/02/90  11.47.02  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGPLOT(X,Y)
*.===========>
*.
*.   Plot a blank point at the position X,Y
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
      LOGICAL ZFS
*.______________________________________
*
      ZFS=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL IZSAV
      CALL ISMK(1)
      CALL ISPMCI(1)
      CALL IPM(1,X,Y)
      CALL IZSET
      ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
