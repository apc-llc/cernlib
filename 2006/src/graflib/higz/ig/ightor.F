*
* $Id: ightor.F,v 1.1.1.1 1996/02/14 13:10:35 mclareni Exp $
*
* $Log: ightor.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:35  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.11/00 23/10/90  14.08.31  by  O.Couet
*-- Author :    O.Couet   08/06/90
      SUBROUTINE IGHTOR(RHI,RLI,RSI,R,G,B)
*.===========>
*.
*.   Convert HLS to RGB
*.
*..==========> (O.Couet)
      REAL IGHR01
*.______________________________________
*
      RH=MAX(MIN(RHI,360.),0.)
      RL=MAX(MIN(RLI,1.),0.)
      RS=MAX(MIN(RSI,1.),0.)
*
      IF(RL.LE.0.5)THEN
         RM2=RL*(1.+RS)
      ELSE
         RM2=RL+RS-RL*RS
      ENDIF
*
      RM1=2.*RL-RM2
*
      IF(RS.EQ.0.)THEN
         R=RL
         G=RL
         B=RL
      ELSE
         R=IGHR01(RM1,RM2,RH+120.)
         G=IGHR01(RM1,RM2,RH)
         B=IGHR01(RM1,RM2,RH-120.)
      ENDIF
*
      END
