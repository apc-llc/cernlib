*
* $Id: csinfh.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: csinfh.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.32  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSINFH(IND,NSIZE,NPIEC,NUSED,NFREE,NPERC,
     *                 NBLOC,AVERBS,DISBS,NASKS,AVSTEP)
      COMMON /MDPOOL/ IQ(6)
      IF(IQ(IND).EQ.0) CALL MDHINI(IND,1)
      NBLOC=0
      NPIEC=1
      NSF=0
      NSFSQ=0
      NASKS=0
      IAV=0
      AVSTEP=0.
      NPERC=0
      AVERBS=0.
      DISBS=0.
      LAV=IND+8
      IBEG=IND+13
      IF(IQ(IND).EQ.-1) GO TO 2
      IBEG=IND+15
      NASKS=IQ(IND+12)
      IF(NASKS) 2,2,1
    1 RASK=NASKS
      AVSTEP=IQ(IND+13)
      AVSTEP=AVSTEP/RASK
    2 NSIZ=IQ(IND+3)
      NAV=IQ(IND+6)-IBEG
      I=IQ(IND+7)
    3 IF(I) 5,5,4
    4 NPIEC=NPIEC+1
      NSIZ=NSIZ+IQ(I+2)
      NAV=NAV+IQ(I+2)-4
      I=IQ(I)
      GO TO 3
    5 I=LAV
    6 I=IQ(I+1)
      IF(I.EQ.LAV) GO TO 7
      NBLOC=NBLOC+1
      LS=-IQ(I)-2
      IAV=IAV+2
      NSF=NSF+LS
      NSFSQ=NSFSQ+LS*LS
      GO TO 6
    7 NAV=NAV-IAV
      NSIZE=NSIZ
      NFREE=NSF
      NUSED=NAV-NSF
      IF(NAV) 9,9,8
    8 RAV=NAV
      RUS=NAV-NSF
      NPERC=RUS/RAV*100.
    9 IF(NBLOC) 11,11,10
   10 RAV=NSF
      RUS=NBLOC
      AVERBS=RAV/RUS
      RAV=NSFSQ
      DISBS=RAV/RUS-AVERBS*AVERBS
   11 RETURN
      END
