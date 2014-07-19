*
* $Id: mllinm.F,v 1.1.1.1 1996/02/26 17:16:42 mclareni Exp $
*
* $Log: mllinm.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:42  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MLLINM(ITABL,IND,LKEY,LINF,LFLAG,ID,IFUNC,JALFR)
      COMMON /MDPOOL/ IQ(6)
      I=MDLOC(IND,14)
      IQ(ITABL)=I
      IQ(I)=14
      IQ(I+1)=IND
      IQ(I+2)=LKEY
      IQ(I+3)=LINF
      IQ(I+4)=LFLAG
      IQ(I+5)=0
      IQ(I+6)=ID
      IF(ID.EQ.0) IQ(I+6)=IND
      IQ(I+7)=-1
      IQ(I+8)=0
      IQ(I+9)=JALFR
      IQ(I+10)=MJSUBR(IFUNC)
      IF(LKEY.EQ.0) GO TO 1
      LOFF=(LKEY+15)/4
      IF(LINF.EQ.0) LOFF=LOFF+1
      IQ(I+11)=LOFF
    1 IQ(I+12)=0
      IQ(I+13)=0
      RETURN
      END
