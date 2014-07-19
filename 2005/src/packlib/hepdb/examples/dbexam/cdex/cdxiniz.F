*
* $Id: cdxiniz.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cdxiniz.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDXINIZ
*     ------------------
*     CDEX Z initialization after CDPAW call
*                             Author :    Boris Khomenko
      IMPLICIT NONE
*
#include "cdxdiv.inc"
*
#include "dbrinf.inc"
*
*     - declare LINK areas
      CALL MZLINK(ICDXDIV,'/CDULNK/',LCDXDIV,LCDXDIV,LCDXDIV)
      IDBRDIV=ICDXDIV
      CALL MZLINK(IDBRDIV,'/DBRLNK/',LDBRKY, LDBRDA, LDBRKY)
*
      END
