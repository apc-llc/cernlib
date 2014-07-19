*
* $Id: mllxsm.F,v 1.1.1.1 1996/02/26 17:16:42 mclareni Exp $
*
* $Log: mllxsm.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:42  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLLXSM(ID,KEY,IP,IT,IS)
      INTEGER CIFNCL
      COMMON /MDPOOL/ IQ(6)
      MLLXSM=0
      LKEY=IQ(ID+2)
      JFUN=IQ(ID+10)
      IT=IQ(ID+7)
      IP=IT
      IS=IQ(IT)
    1 I=IQ(IP)
      IF(I.EQ.0) RETURN
      IPS=IP
      J=I+3
      IF(LKEY.EQ.0) J=J+1
#if defined(CERNLIB_ASSEMB)
      IF(CIFNCL(JFUN,KEY,IQ(J))) 2,5,3
#endif
#if !defined(CERNLIB_ASSEMB)
      IF(CIFNCL(%VAL(JFUN),2,KEY,IQ(J))) 2,5,3
#endif
    2 IP=I
      GO TO 4
    3 IP=I+2
    4 IF(IQ(I+1).EQ.0) GO TO 1
      IT=IPS
      IS=I
      GO TO 1
    5 MLLXSM=I
      RETURN
      END
