*
* $Id: dzdpin.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdpin.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDPIN(KEY,CHOPT)
      CHARACTER*(*)CHOPT
      INTEGER KEY(2)
#include "dzdoc/linout.inc"
#include "dzdoc/bkwrp.inc"
 
      CALL DZDSCO(CHOPT,IFLOPT)
      IF(LBQBKD.NE.0)CALL MZDROP(0,LBQBKD,'I')
      LBQBKD=0
      CALL RZIN(0,LDQBKD,2,KEY,100000,'D')
      LQBKD = LDQBKD
      IF(IQUEST(1) .NE. 0)GOTO 10
*      WRITE(LUNOUT,'(A,2A4)')' DZDPIN for: ',KEY
      IF(LBQBKD.NE.0)CALL MZDROP(0,LBQBKD,'I')
      LBQBKD=0
      CALL DZDGEN
      CALL DZDENT
      CALL MZDROP(0,LDQBKD,'I')
      LDQBKD=0
   10 CONTINUE
      END
***********************************************************************
