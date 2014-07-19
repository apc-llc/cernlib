*
* $Id: cdinic.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cdinic.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDINIC
#include "hepdb/cdtime.inc"
#include "hepdb/cdunit.inc"
#include "hepdb/hdbpara.inc"
#include "hepdb/hdblun.inc"
#include "hepdb/hdbpha.inc"
#include "zebra/zunit.inc"
*
*     General initialisation routine
*
*     Set default log level
*
      LLOGCD = 0
*
*     Set units for output
*
      LPRTCD = IQPRNT
      LOUTCD = IQPRNT
      LFILCD = 9
*
*     Clear logical units
*
      DO 10 I=1,KMXLUN
      MCDLUN(I) = -1
      MCDLUA(I) = -1
   10 CONTINUE
*
*     Set program phas
*
      MCDPHA = 0
*
*     Date and time range
*
      IDATCD = 0
      ITIMCD = 0
      JDATCD = 0
      JTIMCD = 0
 
      END
