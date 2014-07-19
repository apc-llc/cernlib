*
* $Id: cdlupp.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdlupp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLUPP(CHTOP,CHPATH,ID,IT,IRC)
      CHARACTER*(*) CHTOP,CHPATH
      CHARACTER*6   TOPDIR
#include "hepdb/cdlupc.inc"
      EXTERNAL CDLUPU
 
      IRC    = 0
      IDMAXU = 0
      ITMAXU = 0
      CHDIRU = ' '
 
      LTOP   = LENOCC(CHTOP)
      IF(LTOP.EQ.2) THEN
         WRITE(TOPDIR,9001) CHTOP(1:LTOP)
9001  FORMAT('//CD',A2)
      ELSEIF(LTOP.EQ.4) THEN
         WRITE(TOPDIR,9002) CHTOP(1:LTOP)
9002  FORMAT('//',A4)
      ELSE
         TOPDIR = CHTOP
      ENDIF
 
      CALL CDSCAN(TOPDIR,99,CDLUPU,IRC)
 
      END
 
