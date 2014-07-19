*
* $Id: mdfh.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdfh.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.01  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MDFH(IND,ID)
      COMMON /MDPOOL/ IQ(6)
      KD=ID-1
      N=IQ(KD)
      IF(N) 1,1,2
    1 CALL MDPAUS('?MDFH: ILLEGAL REFERENCE TO DYNAMIC AREA')
      GO TO 11
    2 I1=KD+N
      IF(IQ(I1)) 5,3,3
    3 IF(IQ(KD-1)) 9,4,4
    4 LF=IQ(IND+9)
      LB=IND+8
      GO TO 7
    5 N=N-IQ(I1)
      LF=IQ(I1+1)
      LB=IQ(I1+2)
      IF(I1.NE.IQ(IND+11)) GO TO 6
      IQ(IND+11)=IND+8
    6 I1=I1-IQ(I1)
      IF(IQ(KD-1)) 8,7,7
    7 IQ(KD+1)=LF
      IQ(KD+2)=LB
      IQ(LF+2)=KD
      IQ(LB+1)=KD
      GO TO 10
    8 IQ(LB+1)=LF
      IQ(LF+2)=LB
    9 N=N-IQ(KD-1)
      KD=KD+IQ(KD-1)
   10 IQ(KD)=-N
      IQ(I1-1)=-N
   11 RETURN
      END
