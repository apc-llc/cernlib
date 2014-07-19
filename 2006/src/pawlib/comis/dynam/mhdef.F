*
* $Id: mhdef.F,v 1.1.1.1 1996/02/26 17:16:59 mclareni Exp $
*
* $Log: mhdef.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:59  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE MHDEF(N,MEXP)
      EXTERNAL MDLH,MDFH
      COMMON /MDIND/ MSTCK,MHEAP
      MHEAP=MDDEF(0,N,0,MDLH,MDFH,MEXP)
      RETURN
      END
