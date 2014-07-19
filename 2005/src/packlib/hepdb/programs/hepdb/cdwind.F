*
* $Id: cdwind.F,v 1.2 1996/03/12 13:09:13 cernlib Exp $
*
* $Log: cdwind.F,v $
* Revision 1.2  1996/03/12 13:09:13  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDWIND
#include "hepdb/cdtime.inc"
      CHARACTER*80 CHWIND
      CHARACTER*8  CHOPT
 
      CALL KUGETC(CHWIND,LWIND)
      CALL KUGETC(CHOPT,LOPT)
 
      IF(INDEX(CHOPT,'R').NE.0) THEN
         IDATCD = 0
         ITIMCD = 0
         JDATCD = 0
         JTIMCD = 0
      ELSE
         CALL CDDTRN(CHWIND(1:LWIND),IDATCD,ITIMCD,JDATCD,JTIMCD,IC)
      ENDIF
 
      END
