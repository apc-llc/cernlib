*
* $Id: hiex4.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: hiex4.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/01 06/05/93  12.14.48  by  O.Couet
*-- Author :
      SUBROUTINE HIEX4
*.==========>
*.           Examples of graphs, histograms and pie charts
*..=========>
*
      COMMON /QUEST/ RQUEST(100)
      DIMENSION X(10),Y(10),V(10)
      DATA Y/2.,3.,5.,4.,7.,10.,11.,9.,10.,4./
      DATA X/0.,16.,8*0./
      DATA V/-1.5,1.,2.,4.,4.5,6.,9.,10.,14.,17./
*
      CALL IGRNG(15.,18.)
      R  = RQUEST(11)
      XL = RQUEST(12)
      YB = RQUEST(13)
      CALL ICLRWK(0,1)
      CALL ISTXFP(-13,1)
*
      CALL ISWN(10,0.,18.,-1.,12.)
      CALL ISVP(10,8.*R+XL,14.*R+XL,11.*R+YB,17.*R+YB)
      CALL ISELNT(10)
      CALL ISMK(29)
      CALL IGHIST(10,X,Y,'AHCP')
*
      CALL ISWN(20,0.,18.,0.,12.)
      CALL ISVP(20,R+XL,7.*R+XL,11.*R+YB,17.*R+YB)
      CALL ISELNT(20)
      CALL IGHIST(10,X,Y,'AB')
*
      CALL ISWN(30,-4.,19.,-1.,13.)
      CALL ISVP(30,R+XL,14.*R+XL,R+YB,10.*R+YB)
      CALL ISELNT(30)
      CALL IGAXIS(-3.,19.,1.,1.,-3.,19.,20510,' ')
      CALL IGSET('LASI',0.5)
      CALL IGAXIS(-3.,-3.,1.,12.,1.,12.,510,'H')
      CALL ISMK(21)
      CALL IGRAPH(10,V,Y,'LP')
      CALL ISLN(2)
      CALL IGRAPH(10,V,Y,'C')
      CALL IGSET('*',0.)
*
      END
