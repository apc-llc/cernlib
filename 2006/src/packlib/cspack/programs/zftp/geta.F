*
* $Id: geta.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: geta.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE GETA
*
*     Action routine for GETA command
*
#include "cspack/czsock.inc"
      CHARACTER*80 REMOTE,LOCAL
      CHARACTER*8  CHOPT
*
#include "cspack/czopen.inc"
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETS(LOCAL,NCHL)
      NCHL = LENOCC(LOCAL)
      IF(NCHL.LE.0)LOCAL=REMOTE
      IF(LOCAL(1:NCHL).EQ.'=') LOCAL=REMOTE
      NCHL = LENOCC(LOCAL)
      CALL KUGETC(CHOPT,NCH)
      IF(NCH.EQ.0) THEN
         CHOPT = ' '
         NCH   = 1
         ENDIF
#include "cspack/czfold.inc"
      CALL XZGETA(LOCAL,REMOTE,CHOPT,IRC)
99    END
 
