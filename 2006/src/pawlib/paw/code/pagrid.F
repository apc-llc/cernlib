*
* $Id: pagrid.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pagrid.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.05/01 16/12/88  17.11.14  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAGRID
*
*             To draw a grid
*
      DIMENSION X(2),Y(2)
      CHARACTER*3 CH
*
      SIZ=0.25
      CALL ISELNT(1)
      CALL ISCHH(SIZ)
      CALL ISTXAL(1,0)
      CALL HPLSIZ(XSIZ,YSIZ,'R')
      IXSIZ=XSIZ
      IYSIZ=YSIZ
      X0=0.
      DO 10 I=1,IYSIZ-1
        X(1)=1
        X(2)=IXSIZ-1
        Y(1)=I
        Y(2)=I
        CALL IPL(2,X,Y)
        Y0=Y(1)-0.5*SIZ
        WRITE (CH,100) I
        CALL ITX(X0,Y0,CH)
10    CONTINUE
      Y0=SIZ*2
      DO 20 I=1,IXSIZ-1
        X(1)=I
        X(2)=I
        Y(1)=IYSIZ-1
        Y(2)=1
        CALL IPL(2,X,Y)
        X0=X(1)-2*SIZ
        WRITE (CH,100) I
        CALL ITX(X0,Y0,CH)
20    CONTINUE
*
100   FORMAT (I3)
      END
