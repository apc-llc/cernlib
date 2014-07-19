*
* $Id: show_chain.F,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
*
* $Log: show_chain.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/00 25/10/93  13.02.25  by  Rene Brun
*-- Author :    Fons Rademakers   22/10/92
      SUBROUTINE SHOW_CHAIN
*
*  Interface routine to show_chain_tree routine
*
#include "paw/pcchar.inc"
*
      LC = LENOCC(CHTEMP)
      CALL SHOW_CHAIN_TREE(CHTEMP, LC)
*
      END
