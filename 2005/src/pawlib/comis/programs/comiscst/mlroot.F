*
* $Id: mlroot.F,v 1.1.1.1 1996/02/26 17:16:43 mclareni Exp $
*
* $Log: mlroot.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:43  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLROOT(ITABL,IROOT)
      COMMON /MDPOOL/ IQ(6)
      MLROOT=-1
      ID=IQ(ITABL)
      IF(ID.EQ.0) RETURN
      MLROOT=0
      IR=ID+7
      IF(IQ(IR).LT.0) RETURN
      IROOT=IQ(IR)
      MLROOT=1
      RETURN
      END
