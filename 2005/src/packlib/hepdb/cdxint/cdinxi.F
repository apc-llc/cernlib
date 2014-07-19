*
* $Id: cdinxi.F,v 1.1.1.1 1996/02/28 16:24:50 mclareni Exp $
*
* $Log: cdinxi.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:50  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDINXI
*
************************************************************************
*
*        SUBR. CDINXI
*
*   Defines interactive Menus and Commands for HEPDB
*
*   Called by DBMAIN
*
************************************************************************
*
      CALL CDINME
      CALL CDINCD
      CALL CDINAU
      CALL CDINPL
*                                                             END CDINXI
      END
