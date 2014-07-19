*
* $Id: dzepsb.F,v 1.1.1.1 1996/03/04 16:13:24 mclareni Exp $
*
* $Log: dzepsb.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:24  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEPSB(IX1,IY1,IX2,IY2,LUOUT)
      CHARACTER*20 CCINT
      CALL DZDPLN(LUOUT,' n',-2)
      WRITE(CCINT,10)IX1,IY1,' m'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX2,IY1,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX2,IY2,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX1,IY2,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      WRITE(CCINT,10)IX1,IY1,' d'
      CALL DZDPLN(LUOUT,CCINT,-1)
      CALL DZDPLN(LUOUT,' s',0)
      WRITE(LUOUT,*)' s'
10    FORMAT(2I8,A)
      END
****************************************************************
