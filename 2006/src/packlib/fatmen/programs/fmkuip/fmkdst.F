*
* $Id: fmkdst.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmkdst.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKDST
 
#include "fatmen/fatsys.inc"
#include "fatmen/fatsat.inc"
#include "fatmen/fatbug.inc"
 
      CHARACTER*40 DEST
 
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(DEST,LDEST)
 
      IF(LDEST.EQ.0) THEN
         CALL FMSDST
         RETURN
      ENDIF
 
      PRINT *,'FMKDST. setting destination site to ',DEST(1:LDEST)
 
      LDSTST = LDEST
      CHDEST = DEST(1:LDEST)
 
      JX     = ICNTH(CHDEST(1:LDSTST),CHSITE,NSITES)
      IF(JX.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'*** warning - site specified not ',
     +      'found in list of known sites ***'
      ENDIF
*
*     Check that source and destination are not the same
*
      IF(LSRCST.NE.0.AND.CHSRCE(1:LSRCST).EQ.CHDEST(1:LDEST)) THEN
         IF(IDEBFA.GE.0) PRINT *,'*** warning - source and destination',
     +      ' sites are identical ***'
      ENDIF
 
      END
