*
* $Id: csadecb.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: csadecb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 01/11/94  12.15.53  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   31/10/94
      SUBROUTINE CSADECB(ISFTG,IL)
*----------------------------------------
*    add element to the end of the list
*  common blocks list:  next;listgi;numgb
*  common block element:next;iptli
*---------------------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
*+SEQ,CSTAB.
      I=MHLOC(2)
      IQ(ISFTG)=I
      IQ(I)=0
      IQ(I+1)=IL
      ISFTG=I
      END
