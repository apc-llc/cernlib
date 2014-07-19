*
* $Id: fmksrc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmksrc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKSRC
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatsat.inc"
#include "fatmen/fatbug.inc"
 
      CHARACTER*40 SOURCE
 
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(SOURCE,LSRC)
 
      IF(LSRC.EQ.0) THEN
         CALL FMSSRC
         RETURN
      ENDIF
 
      PRINT *,'FMKSRC. setting source site to ',SOURCE(1:LSRC)
 
      LSRCST = LSRC
      CHSRCE = SOURCE(1:LSRC)
 
      JX     = ICNTH(CHSRCE(1:LSRCST),CHSITE,NSITES)
      IF(JX.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'*** warning - site specified not ',
     +      'found in list of known sites ***'
      ENDIF
*
*     Check that source and destination are not the same
*
      IF(LDSTST.NE.0.AND.CHSRCE(1:LSRCST).EQ.CHDEST(1:LDSTST)) THEN
         IF(IDEBFA.GE.0) PRINT *,'*** warning - source and destination',
     +      ' sites are identical ***'
      ENDIF
 
      END
