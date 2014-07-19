*
* $Id: igbord.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: igbord.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 15/06/89  17.36.38  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGBORD(NTB,X1,X2,Y1,Y2,ILNTY)
*.===========>
*.
*.   This routine draws the curent NT border
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION X(5),Y(5)
      LOGICAL ZFS,GFS
*.______________________________________
*
      CALL IZSAV
      NTSAV=INTR
      ZFS=ZFLAG
      GFS=GFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      X(1)=X1
      X(2)=X1
      X(3)=X2
      X(4)=X2
      X(5)=X1
      Y(1)=Y1
      Y(2)=Y2
      Y(3)=Y2
      Y(4)=Y1
      Y(5)=Y1
*
      IF(NTB.EQ.0)THEN
         X(1)=0.
         X(2)=0.
         X(3)=1.
         X(4)=1.
         X(5)=0.
         Y(1)=0.
         Y(2)=(RDWYMA-RDWYMI)*REDIT
         Y(3)=(RDWYMA-RDWYMI)*REDIT
         Y(4)=0.
         Y(5)=0.
      ENDIF
      CALL ISELNT(NTB)
      CALL ISFACI(0)
      CALL ISFAIS(1)
      CALL ISTXFP(0,2)
      CALL IGBOX(X1,X2,Y1,Y2)
      CALL IGAXIS(X1,X2,Y1,Y1,X1,X2,1,'NB')
      CALL IGAXIS(X1,X1,Y1,Y2,Y1,Y2,1,'NB')
      IF(NTB.EQ.NTSAV)THEN
         CALL ISMKSC(2.)
         CALL ISMK(29)
         XP=(X2-X1)/2
         YP=(Y2-Y1)/2
         CALL IPM(1,XP,YP)
      ENDIF
      CALL ISPLCI(1)
      CALL ISLN(ILNTY)
      CALL IPL(5,X,Y)
      CALL IZSET
      CALL ISELNT(NTSAV)
      ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
*
      END
