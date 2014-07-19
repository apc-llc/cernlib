*
* $Id: putc.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: putc.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.34.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE PUTC(NAME,IADR)
************************************************
*                                              *
*   SYSTEM-INDEPENDENT ROUTINE: PUT THE RECORD *
*   CONTAINING THE COMMON-BLOCK NAME INTO THE  *
*   TABLE                                      *
*                                              *
*   NAME - ARRAY OF TWO WORDS, CONTAINING THE  *
*          NAME OF THE COMMON-BLOCK ( WITH     *
*          POSSIBLE TRAILING BLANKS ),         *
*                                              *
*   IADR - THE ADDRESS OF THE COMMON-BLOCK     *
*          ( IN TERMS OF 'MJWORD' )             *
*                                              *
************************************************
      COMMON /MDPOOL/ IQ(6)
#include "comis/cstbls.inc"
      DIMENSION NAME(2)
*
      I=MLINS(ITBLC,NAME)
      IF(I.GT.0) IQ(I)=IADR
      RETURN
      END
