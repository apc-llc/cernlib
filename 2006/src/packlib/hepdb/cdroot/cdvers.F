*
* $Id: cdvers.F,v 1.4 1996/04/12 09:42:21 cernlib Exp $
*
* $Log: cdvers.F,v $
* Revision 1.4  1996/04/12 09:42:21  cernlib
* New handling of title
*
* Revision 1.3  1996/03/29 11:18:12  jamie
* qftitlch
*
* Revision 1.2  1996/03/28 10:37:20  jamie
* update IDATQQ/ITIMQQ
*
* Revision 1.1.1.1  1996/02/28 16:24:32  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDVERS(CHTITL,CHCOMP,CHOPT)
*
*     Print or return HEPDB version number
*
      CHARACTER*(*) CHTITL,CHCOMP,CHOPT
      CHARACTER*80  CTITLE,CCOMPL
 
#include "hepdb/cduscm.inc"
#include "zebra/zunit.inc"
 
      IDATQQ = 960328
      ITIMQQ = 1130
*
*     Clear title and compilation date/time
*
      CTITLE = ' '
      CCOMPL = ' '
 
#include "hepdb/qftitlch.inc"
      CTITLE =
     + HepdbTitleFortranString
      WRITE(CCOMPL,9001) IDATQQ,ITIMQQ
9001  FORMAT(' This version created on ',I6,' at ',I4)
 
      LTITLE = LENOCC(CTITLE)
      LCOMPL = LENOCC(CCOMPL)
 
      IF(INDEX(CHOPT,'P').NE.0) THEN
 
         WRITE(LPRTCD,*) CTITLE(1:LTITLE)
         WRITE(LPRTCD,*) CCOMPL(1:LCOMPL)
 
      ELSE
 
         CHTITL = CTITLE(1:LTITLE)
         CHCOMP = CCOMPL(1:LCOMPL)
 
      ENDIF
 
      END
