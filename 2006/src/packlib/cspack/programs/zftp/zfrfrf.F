*
* $Id: zfrfrf.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zfrfrf.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFRFRF
*
*     Action routine for RFRF command
*
#include "cspack/czsock.inc"
#include "cspack/quest.inc"
      CHARACTER*64 LOCAL,REMOTE
      CHARACTER*4  CHOPT
 
      CALL KUGETS(LOCAL,NCHL)
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETI(LRECL)
      CALL KUGETI(NQUOTA)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
 
      IF(NQUOTA.NE.0) THEN
         CHOPT(NCH+1:NCH+1) = 'Q'
         IQUEST(10)         = NQUOTA
      ELSE
         IQUEST(10)         = 0
      ENDIF
 
      CALL XZRFRF(LOCAL,REMOTE,LRECL,CHOPT,IRC)
      IF(IRC.NE.0) PRINT 9001,IRC
9001  FORMAT(' Return code ',I10,' from XZRFRF')
99    END
 
