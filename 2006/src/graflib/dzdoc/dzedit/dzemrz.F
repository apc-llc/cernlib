*
* $Id: dzemrz.F,v 1.1.1.1 1996/03/04 16:13:21 mclareni Exp $
*
* $Log: dzemrz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:21  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEMRZ
 
C--     Make a new RZ file
 
#include "dzeditkeep.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
*     open files
 
      CALL KUGETS(CHCARD,NCHA)
      CALL KUGETS(CHRZF,NCHA)
      CALL KUCMD('DZEDIT',' ','SW')
      CALL KUPVAL('LISTDIRECTORY','CHRZF',0,0.,CHRZF,'D')
      CALL KUPVAL('OPENRZFILE','CHRZF',0,0.,CHRZF,'D')
      CALL KUCMD (BS,' ','SW')
      CALL DZDMRZ(LUPCAR,LUPRZ,CHCARD,CHRZF,CHSUBD,IRET)
      IF(IRET.EQ.0)LUNRZ=LUPRZ
      END
*************************************************************
