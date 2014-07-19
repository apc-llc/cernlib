*
* $Id: rzvers.F,v 1.2 1999/06/14 13:59:31 couet Exp $
*
* $Log: rzvers.F,v $
* Revision 1.2  1999/06/14 13:59:31  couet
* - Mods for Y2K in the date/time output
*
* Revision 1.1.1.1  1996/03/06 10:47:23  mclareni
* Zebra
*
*
#include "zebra/pilot.h"
      SUBROUTINE RZVERS(CHTITL,CHCOMP,CHOPT)
*
*     Print or return RZ version number
*
      CHARACTER*(*) CHTITL,CHCOMP,CHOPT
      CHARACTER*80  CTITLE,CCOMPL
 
#include "zebra/zunit.inc"
 
      IDATQQ = 0
      ITIMQQ = 0
*
*     Clear title and compilation date/time
*
      CTITLE = ' '
      CCOMPL = ' '
 
      CTITLE =
     +  'RZEBRA   3.01/08'
      WRITE(CCOMPL,9001) IDATQQ,ITIMQQ
9001  FORMAT(' This version created on ',I6.6,' at ',I4.4)
 
      LTITLE = LENOCC(CTITLE)
      LCOMPL = LENOCC(CCOMPL)
 
      IF(INDEX(CHOPT,'P').NE.0) THEN
 
         WRITE(IQPRNT,*) CTITLE(1:LTITLE)
         WRITE(IQPRNT,*) CCOMPL(1:LCOMPL)
 
      ELSE
 
         CHTITL = CTITLE(1:LTITLE)
         CHCOMP = CCOMPL(1:LCOMPL)
 
      ENDIF
 
      END
