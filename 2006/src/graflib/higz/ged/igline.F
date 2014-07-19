*
* $Id: igline.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: igline.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 16/12/88  14.00.47  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGLINE(XS,XE,YS,YE)
*.===========>
*.
*.   Draw a line between two points
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
      DIMENSION X(2),Y(2)
      LOGICAL GFS,ZFS
*.______________________________________
*
      GFS=GFLAG
      ZFS=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
*
      CALL IZSAV
      X(1)=XS
      X(2)=XE
      Y(1)=YS
      Y(2)=YE
      CALL ISLN(1)
      CALL ISPLCI(1)
      CALL IPL(2,X,Y)
      CALL IZSET
*
      ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
*
      END
