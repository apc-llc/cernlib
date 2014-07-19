*
* $Id: igcles.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igcles.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 05/04/93  10.44.38  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCLES
*.===========>
*.
*.   This routine clears the editing space
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION X(3),Y(3)
      LOGICAL ZFS,GFS,PFS
*.______________________________________
*
      CALL IZSAV
      ZFS=ZFLAG
      GFS=GFLAG
      PFS=PFLAG
      ZFLAG=.FALSE.
      PFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
*
      ICLIPS=ICLIP
      REDITS=REDIT
      CALL IGSRAP(0.)
      CALL ISCLIP(0)
      CALL ISELNT(0)
*
      CALL IGQWK(1,'MXDS',X)
      X2CL=MIN(X(1)/X(2),1.)*REDITS
      Y2CL=MIN(X(2)/X(1),1.)*REDITS
      X(1)=0.
      X(2)=X2CL
      X(3)=X2CL
      Y(1)=Y2CL
      Y(2)=Y2CL
      Y(3)=0.
      CALL IGSET('BORD',0.)
      CALL ISFACI(0)
      CALL ISFAIS(1)
      CALL ISLN(1)
      CALL ISPLCI(1)
      CALL IGBOX(0.,X2CL,0.,Y2CL)
      CALL IPL(3,X,Y)
*
      CALL IGSRAP(REDITS)
      CALL ISCLIP(ICLIPS)
      CALL IZSET
      ZFLAG=ZFS
      PFLAG=PFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
*
      END

