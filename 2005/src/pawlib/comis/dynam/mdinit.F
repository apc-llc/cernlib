*
* $Id: mdinit.F,v 1.1.1.1 1996/02/26 17:16:56 mclareni Exp $
*
* $Log: mdinit.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.15.13  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MDINIT(N,MEXP)
#include "comis/cslun.inc"
#include "comis/cssysd.inc"
      COMMON /MDPOOL/ IQ(6)
      COMMON /CSTBCS/ITBC,ITBS
      COMMON/CSLBRW/LIBDEF(5)
      COMMON /MPCNST/ NFRC,NRSIZ,NPAG
      EXTERNAL MEXP
*
      CALL CSSETL(0,0,0,0,0)
      ISTARR(1)=0
      ISTARR(2)=0
      ISTARR(3)=0
      ISTARR(4)=0
      ISTARR(5)=0
      LIBDEF(1)=0
      LIBDEF(2)=0
      LIBDEF(3)=98938
      LIBDEF(4)=1
      LIBDEF(5)=4
      ITBC=0
      ITBS=0
      NWPR=1
      NBITPW=32
      NBYTPW=4
      NFRC=1
      NRSIZ=127
      NPAG=2
      M=N
      IF(M.LT.6) M=6
      IQ(1)=5
      IQ(2)=M-6
      IQ(3)=JUMPAD(MEXP)
      IQ(4)=0
      IQ(5)=0
      IQ(6)=0
      RETURN
      END
