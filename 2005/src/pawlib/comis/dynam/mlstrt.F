*
* $Id: mlstrt.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mlstrt.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLSTRT(ITABL,IROOT)
      COMMON /MDPOOL/ IQ(6)
      MLSTRT=-1
      ID=IQ(ITABL)
      IF(ID.EQ.0) RETURN
      MLSTRT=0
      IR=ID+7
      IF(IQ(IR).GE.0) RETURN
      IQ(IR)=IROOT
      MLSTRT=1
      RETURN
      END
