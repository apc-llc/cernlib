*
* $Id: dzedff.F,v 1.1.1.1 1996/03/04 16:13:23 mclareni Exp $
*
* $Log: dzedff.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEDFF
#include "dzeditkeep.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
 
*      set default parameters
 
      CALL KUCMD ('DZEDIT','   ','SW')
 
      CALL KUPVAL('CREATEDOC'    ,'CHCARD',0,0.,CHCARD,'D')
      CALL KUPVAL('CREATEDOC'    ,'CHRZF',0,0.,CHRZF,'D')
      CALL KUPVAL('UPDATEDOC'    ,'CHCARD',0,0.,CHCARD,'D')
      CALL KUPVAL('UPDATEDOC'    ,'CHRZF',0,0.,CHRZF,'D')
      CALL KUPVAL('UPDATEDOC'    ,'CHSUBD',0,0.,CHSUBD,'D')
      CALL KUPVAL('OPENRZFILE'   ,'CHRZF',0,0.,CHRZF,'D')
      CALL KUPVAL('DRAWONETREE'  ,'CHMETA',0,0.,CHMETA,'D')
      CALL KUPVAL('DRAWONETREE'  ,'CHSGML',0,0.,CHSGML,'D')
      CALL KUPVAL('DRAWONETREE'  ,'CTITLE',0,0.,CTITLE,'D')
      CALL KUPVAL('DRAWONETREE'  ,'CHOPT',0,0.,'P','D')
      CALL KUPVAL('DRAWALL'      ,'CHMETA',0,0.,CHMETA,'D')
      CALL KUPVAL('DRAWALL'      ,'CHSGML',0,0.,CHSGML,'D')
      CALL KUPVAL('DRAWALL'      ,'CTITLE',0,0.,CTITLE,'D')
      CALL KUPVAL('DRAWALL'      ,'CHOPT',0,0.,'P','D')
      CALL KUPVAL('LISTONEBANK'  ,'CHLIST',0,0.,CHLIST,'D')
      CALL KUPVAL('LISTONEBANK'  ,'CHBSBK',0,0.,CHBSBK,'D')
      CALL KUPVAL('LISTONEBANK'  ,'CHBSUP',0,0.,CHBSUP,'D')
      CALL KUPVAL('LISTALL'      ,'CHLIST',0,0.,CHLIST,'D')
      CALL KUCMD (BS,'   ','SW')
      END
 
***********************************************************************
