*
* $Id: pacsel.F,v 1.1.1.1 1996/03/01 11:39:26 mclareni Exp $
*
* $Log: pacsel.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/07 29/11/93  12.39.06  by  Fons Rademakers
*-- Author :    Fons Rademakers   22/07/93
      SUBROUTINE PACSEL
************************************************************************
*                                                                      *
*  PACSEL stub called by Piaf server.                                  *
*                                                                      *
************************************************************************
*
#include "paw/pccsel.inc"
*
      CALL CZPUTA('PACSEL', ISTAT)
      CALL CZPUTA(CHCSEL, ISTAT)
*
      END
