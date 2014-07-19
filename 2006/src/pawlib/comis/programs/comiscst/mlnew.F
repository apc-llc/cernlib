*
* $Id: mlnew.F,v 1.1.1.1 1996/02/26 17:16:44 mclareni Exp $
*
* $Log: mlnew.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:44  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLNEW(IND,ITABL)
      COMMON /MDPOOL/ IQ(6)
      IT=MDLOC(IND,6)
      CALL CCOPYA(IQ(ITABL),IQ(IT),6)
      IQ(IT)=0
      ID=IQ(ITABL)
      IF(ID.EQ.0) GO TO 1
      LEN=IQ(ID)
      IDN=MDLOC(IND,LEN)
      IQ(IT)=IDN
      CALL CCOPYA(IQ(ID),IQ(IDN),LEN)
      IQ(IDN+1)=IND
      IF(IQ(ID+6).EQ.IQ(ID+1)) IQ(IDN+6)=IND
      IQ(IDN+7)=-1
      IQ(IDN+8)=0
    1 MLNEW=IT
      RETURN
      END
