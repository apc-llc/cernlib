*
* $Id: dzeurz.F,v 1.1.1.1 1996/03/04 16:13:20 mclareni Exp $
*
* $Log: dzeurz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:20  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEURZ
#include "dzeditkeep.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
 
C--     Update existing RZ file
 
*     get parameter from KUIP command line
 
      CALL KUGETS(CHCARD,NCHA)
      IF(NCHA.LE.0)GOTO 999
      CALL KUGETS(CHRZF,NCHA)
      IF(NCHA.LE.0)GOTO 999
      CALL KUCMD('DZEDIT',' ','SW')
      CALL KUPVAL('LISTDIRECTORY','CHRZF',0,0.,CHRZF,'D')
      CALL KUPVAL('OPENRZFILE','CHRZF',0,0.,CHRZF,'D')
      CALL KUCMD (BS,' ','SW')
      CALL  DZDURZ(LUPCAR,LUPRZ,CHCARD,CHRZF,CHSUBD,IRET)
      IF(IRET.EQ.0)LUNRZ=LUPRZ
999   END
*********************************************************************
