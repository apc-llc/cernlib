*
* $Id: igsmp.F,v 1.2 1998/01/30 15:22:49 couet Exp $
*
* $Log: igsmp.F,v $
* Revision 1.2  1998/01/30 15:22:49  couet
* - APOLLO version removed
*
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  19.00.04  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGSMP(IOPT)
*.===========>
*.
*.   This routine sets the menu parameters
*.
*..==========> (O.Couet)
#include "higz/himenu.inc"
#include "higz/hiatt.inc"
      DIMENSION IOPT(*)
*.______________________________________
*
      IF(XPOS(1).GT.RDWXMA)XPOS(1)=RDWXMA
      IF(XPOS(2).GT.RDWXMA)XPOS(2)=RDWXMA
      IF(XPOS(1).LT.0)XPOS(1)=0.
      IF(XPOS(2).LT.0)XPOS(2)=0.
      IF(YPOS(1).GT.RDWYMA)YPOS(1)=RDWYMA
      IF(YPOS(2).GT.RDWYMA)YPOS(2)=RDWYMA
      IF(YPOS(1).LT.0)YPOS(1)=0.
      IF(YPOS(2).LT.0)YPOS(2)=0.
*
      XSIZ=XPOS(2)-XPOS(1)
      NDIV=NBCHOI
      IF(IOPT(13).EQ.0)NDIV=NDIV+1
      IF(NBUSER.NE.0)NDIV=NDIV+1
      RINT=(YPOS(2)-YPOS(1))/FLOAT(NDIV)
      DEC=RINT/6.
*
      IF(IOPT(13).NE.0)THEN
         Y3=YPOS(2)
      ELSE
         Y3=YPOS(2)-RINT
      ENDIF
*
      IF(NBUSER.NE.0)THEN
         Y4=YPOS(1)+RINT
         USIZ=(XSIZ-IOPT(19)*(NBUSER-1)*10.*DEC)/NBUSER
      ELSE
         Y4=YPOS(1)
         USIZ=0.
      ENDIF
*
      YSIZ=RINT
      XM=XPOS(1)+(XSIZ/2.)
      IF(IOPT(5).NE.0)XSIZ=XSIZ/2.
*
      UTH=0.66*RINT
      TH=UTH
      OTH=UTH
*
      YT=YPOS(2)-(YSIZ/2.)-(TH/2.)
      YSTEP=(YSIZ/2.)+(OTH/2.)
*
      END

