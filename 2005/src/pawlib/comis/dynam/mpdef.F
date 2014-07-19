*
* $Id: mpdef.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mpdef.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MPDEF(IDC,IDP,IARRAY)
      COMMON /MPCNST/NFRC,NRSIZC,NPAGC
      COMMON /MDPOOL/ IQ(6)
      COMMON /MPLIST/ INDP
      DIMENSION IARRAY(5)
      IN=MDLOC(IDC,16)
      MPDEF=IN
      CALL CCOPYA(IARRAY(1),IQ(IN),5)
      IF(IQ(IN+1).LE.0) IQ(IN+1)=NFRC
      IF(IQ(IN+3).LE.0) IQ(IN+3)=NRSIZC
      IF(IQ(IN+4).LE.0) IQ(IN+4)=NPAGC
      IQ(IN+5)=IDP
      CALL VFILL(IQ(IN+6),7,0)
      IQ(IN+13)=IDC
      IQ(IN+14)=INDP
      INDP=IN
      IQ(IN+15)=0
      RETURN
      END
