*
* $Id: hiex2.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: hiex2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/00 10/07/92  09.47.37  by  O.Couet
*-- Author :
      SUBROUTINE HIEX2
*.==========>
*.           Example to plot the table of HIGZ software characters
*..=========>
*
      CHARACTER*6 KD1,KD2
      CHARACTER*45 KDG
      CHARACTER*3 KTEXT
      CHARACTER*1 CHOPT
      DIMENSION XPOS(6),X(5),Y(5)
      DATA KD1/' < < <'/
      DATA KD2/'  [[""'/
      DATA KDG/'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,+-*/=()'/
      DATA XLONG,YTOP/16.,24./
      DATA SIZE,ANGLE/0.3,0./
*
      CALL IGRNG(20.,24.)
      CALL ICLRWK(0,1)
*
      XW = XLONG/12.
      DO 10 I = 1,6
         XPOS(I) = (2*I-1)*XW + 2.5
  10  CONTINUE
*
*              Draw the frame
*
      YLONG  = 46*1.5*SIZE + 5*1.5*SIZE
      X(1)   = XPOS(1) - XW
      X(2)   = XPOS(6) + XW
      X(3)   = X(2)
      X(4)   = X(1)
      X(5)   = X(1)
      Y(1)   = YTOP
      Y(2)   = Y(1)
      Y(3)   = Y(1) - YLONG
      Y(4)   = Y(3)
      Y(5)   = Y(1)
      CALL IPL(5,X,Y)
      DO 20 I = 1,5
         X(1)   = XPOS(I) + XW
         X(2)   = X(1)
         Y(1)   = YTOP
         Y(2)   = Y(1) - YLONG
         CALL IPL(2,X,Y)
  20  CONTINUE
      X(1)   = XPOS(1) - XW
      X(2)   = XPOS(6) + XW
      Y(1)   = YTOP - 5.*SIZE
      Y(2)   = Y(1)
      CALL IPL(2,X,Y)
*
*             Draw box titles
*
      Y1     = YTOP - 2.*SIZE
      Y2     = Y1 - 2.*SIZE
      CHOPT='C'
      CALL IGTEXT(XPOS(1),Y1,'Upper'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(1),Y2,'Roman'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(2),Y1,'Lower'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(2),Y2,'Roman'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(3),Y1,'Upper'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(3),Y2,'Greek'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(4),Y1,'L<OWER' ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(4),Y2,'G<REEK' ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(5),Y1,'U<PPER' ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(5),Y2,'Special',SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(6),Y1,'Lower'  ,SIZE,ANGLE,CHOPT)
      CALL IGTEXT(XPOS(6),Y2,'Special',SIZE,ANGLE,CHOPT)
*
      YP = YTOP - 6.*SIZE
      DO 40 I = 1,45
         YP = YP - 1.5*SIZE
         DO 30 J = 1,6
            KTEXT=KD1(J:J)//KD2(J:J)//KDG(I:I)
            CALL IGTEXT(XPOS(J),YP,KTEXT,SIZE,ANGLE,CHOPT)
  30     CONTINUE
  40  CONTINUE
*
      END
