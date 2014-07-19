*
* $Id: iggch.F,v 1.1.1.1 1996/02/14 13:11:15 mclareni Exp $
*
* $Log: iggch.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 20/07/89  11.49.13  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGGCH(XPI,YPI,ICH,IOPT)
*.===========>
*.
*.   This routine gets the choice number.
*.
*. ICH=0 : out of the menu
*. ICH=-100 : title bar
*. ICH=-101 : left square
*. ICH=-102 : right square
*. ICH=-1... -NBUSER : user keys
*. ICH= integer > 0 : option number
*.
*..==========> (O.Couet)
#include "higz/himenu.inc"
      DIMENSION IOPT(*)
*.______________________________________
*
      IF((XPI.GT.XPOS(2)).OR.(XPI.LT.XPOS(1))
     +                  .OR.
     +   (YPI.GT.YPOS(2)).OR.(YPI.LT.YPOS(1)))THEN
         ICH=0
         RETURN
      ENDIF
*
      IF(YPI.GT.Y3)THEN
         IF(XPI.LT.XPOS(1)+RINT)THEN
            ICH=-101
            RETURN
         ENDIF
         IF(XPI.GT.XPOS(2)-RINT)THEN
            ICH=-102
            RETURN
         ENDIF
         ICH=-100
         RETURN
      ENDIF
*
      IF(YPI.LT.Y4)THEN
         IF(IOPT(19).NE.0)THEN
            TUSIZ=USIZ+10.*DEC
            ITCH=INT((XPI-XPOS(1))/TUSIZ)+1
            ULEN=ITCH*USIZ+(ITCH-1)*10.*DEC
            IF(XPI.GT.ULEN+XPOS(1))THEN
               ICH=0
            ELSE
               ICH=-ITCH
            ENDIF
         ELSE
            ICH=-(INT((XPI-XPOS(1))/USIZ)+1)
         ENDIF
         RETURN
      ENDIF
*
      ICH=NBCHOI-INT((YPI-Y4)/YSIZ)
*
      END

