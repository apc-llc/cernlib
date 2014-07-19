*
* $Id: getb.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: getb.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE GETB
*
*         Action routine for GETB command
*
      CHARACTER*80 REMOTE,LOCAL
      CHARACTER*8  CHOPT
#include "cspack/czsock.inc"
*
#include "cspack/czopen.inc"
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETS(LOCAL,NCHL)
      IF(NCHL.LE.0)LOCAL=REMOTE
      IF(LOCAL(1:NCHL).EQ.'=') LOCAL=REMOTE
      NCHL = LENOCC(LOCAL)
      CALL KUGETI(LRECL)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
      CALL XZGETB(LOCAL,REMOTE,LRECL,CHOPT,IRC)
99    END
 
