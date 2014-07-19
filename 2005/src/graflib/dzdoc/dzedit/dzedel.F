*
* $Id: dzedel.F,v 1.1.1.1 1996/03/04 16:13:22 mclareni Exp $
*
* $Log: dzedel.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:22  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEDEL
*     delete
#include "dzeditkeep.inc"
      CHARACTER*4 CHDOPT
      INTEGER KEYDEL(2)
      CALL KUGETC(CHBSBK,NCHA)
      IF(NCHA.LT.4)CHBSBK(NCHA:4)=' '
      CALL KUGETC(CHBSUP,NCHA)
      IF(NCHA.LT.4)CHBSUP(NCHA:4)=' '
      CALL UCTOH(CHBSBK,KEYDEL(1),4,4)
      CALL UCTOH(CHBSUP,KEYDEL(2),4,4)
      CALL KUGETI(ICYCLE)
      CALL KUGETC(CHDOPT,NCLOPT)
      CALL RZDELK(KEYDEL,ICYCLE,CHDOPT)
*      IF(IQUEST(1).NE.0)WRITE(*,*)' Key/cycle not found'
      WRITE(*,*)' Maximum number of cycles deleted:',IQUEST(11)
      END
**********************************************************************
 
