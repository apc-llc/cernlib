*
* $Id: hiex3.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: hiex3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/01 18/08/92  10.33.40  by  O.Couet
*-- Author :
      SUBROUTINE HIEX3
*.==========>
*.           Advanced example to draw text
*.           (based on a PAW macro from W.Walk)
*..=========>
*
      DIMENSION X(3),Y(3)
*
      CALL IGRNG(14.6,18.)
      CALL ICLRWK(0,1)
      CALL IGBOX(0.,14.6,0.,18.)
      CALL IGSET('PASS',10.)
      CALL IGSET('CSHI',0.005)
      CALL ISFAIS(1)
      CALL ISTXCI(1)
      CALL ISTXFP(-13,1)
      CALL ISCHH(0.6)
      CALL ISTXAL(2,0)
      CALL ITX(7.3,17.,'Exclusive Toponium Decays')
      CALL ISTXFP(0,2)
      CALL ISFACI(1)
      CALL IGBOX(5.,7.,15.,14.9)
      CALL IGBOX(5.,7.,3.,2.9)
      CALL IGBOX(3.,5.,14.,13.9)
      CALL IGBOX(3.,5.,2.,1.9)
      CALL IGBOX(10.,12.,13.,12.9)
      CALL IGBOX(10.,12.,12.,11.9)
      CALL IGBOX(10.,12.,11.,10.9)
      CALL IGBOX(6.,8.,12.4,12.3)
      CALL ISPLCI(3)
      X(1)=6.
      X(2)=11.
      X(3)=6.
      Y(1)=15.
      Y(2)=13.
      Y(3)=3.
      CALL IPL(3,X,Y)
      Y(2)=12.
      CALL IPL(3,X,Y)
      Y(2)=11.
      CALL IPL(3,X,Y)
      CALL ISPLCI(2)
      X(2)=4.
      Y(2)=14.
      CALL IPL(3,X,Y)
      Y(2)=2.
      CALL IPL(3,X,Y)
      CALL ISPLCI(4)
      X(2)=X(3)
      Y(2)=1.5
      CALL IPL(2,X(2),Y(2))
      X(1)=X(2)-0.2
      X(3)=X(2)+0.2
      Y(1)=Y(2)+0.3
      Y(3)=Y(1)
      CALL IPL(3,X,Y)
      CALL ISTXCI(4)
      CALL IGTEXT(6.,0.5,'e^+!e^-! or [m]^+![m]^-!',0.5,0.,'C')
      CALL IGTEXT(6.,15.2,'2^3!S?1--!',0.5,0.,'C')
      CALL IGTEXT(6.,3.2,'1^3!S?1--!',0.5,0.,'C')
      CALL IGTEXT(11.,13.2,'1^3!P?2++!',0.5,0.,'C')
      CALL IGTEXT(11.,12.2,'1^3!P?1++!',0.5,0.,'C')
      CALL IGTEXT(11.,11.2,'1^3!P?0++!',0.5,0.,'C')
      CALL IGTEXT(7.,12.6,'1^1!P?1+-!',0.5,0.,'C')
      CALL IGTEXT(4.,14.2,'2^1!S?0-+!',0.5,0.,'C')
      CALL IGTEXT(4., 2.2,'1^1!S?0-+!',0.5,0.,'C')
      CALL ISTXCI(6)
      CALL IGTEXT(4.5,15.,'[Q]?2S!',0.5,0.,'R')
      CALL IGTEXT(7.5,2.75,'[Q]?1S! (80 GeV)',0.5,0.,'L')
      CALL IGTEXT(2.5,13.75,'[c]?t!&^,!',0.5,0.,'R')
      CALL IGTEXT(2.5,1.75,'[c]?t!',0.5,0.,'R')
      CALL IGTEXT(12.5,13.,'[h]^2!&?t!',0.5,0.,'L')
      CALL IGTEXT(12.5,12.,'[h]^1!&?t!',0.5,0.,'L')
      CALL IGTEXT(12.5,11.,'[h]^0!&?t!',0.5,0.,'L')
      CALL ISTXCI(3)
      CALL IGTEXT(1.,9.,'E1',0.5,0.,'C')
      CALL ISTXCI(2)
      CALL IGTEXT(3.,9.,'M1',0.5,0.,'C')
      CALL ISTXCI(3)
      CALL IGTEXT(8.8,14.8,'100 MeV',0.4,0.,'L')
      CALL IGTEXT(8.5,6.,'800 MeV',0.4,0.,'L')
      CALL ISTXCI(6)
      CALL IGTEXT(9.4,14.2,'BR 2"Y',0.3,0.,'L')
      CALL IGTEXT(8.9,5.4,'BR 30"Y',0.3,0.,'L')
      CALL IGSET('*',0.)
*
      END
