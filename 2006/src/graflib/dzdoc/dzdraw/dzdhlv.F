*
* $Id: dzdhlv.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzdhlv.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDHLV
      WRITE(*,'(10(A/))')
     &' -----------------------------------------',
     &' Activate locator at the following places:',
     &' -----------------------------------------',
     &' ',
     &' In divisions display      => call DZDISP for choosen bank',
     &' In store display (at top) => call DZDDIV for choosen div',
     &' In Zoom - box             => zoom selected region of div',
     &'                              2 locator inputs are expected',
     &' In the menu boxes to select stores and divisions'
      END
**********************************************************************
