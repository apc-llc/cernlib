*
* $Id: getfz.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: getfz.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE GETFZ
*
*         Action routine for GETFZ command
*
#include "cspack/czsock.inc"
      CHARACTER*80 REMOTE,LOCAL
      CHARACTER*4  CHOPT,LFORM,RFORM
      INTEGER      RRECL
#include "cspack/czopen.inc"
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETS(LOCAL,NCHL)
      IF(NCHL.LE.0)LOCAL=REMOTE
      IF(LOCAL(1:NCHL).EQ.'=') LOCAL=REMOTE
      NCHL = LENOCC(LOCAL)
      CALL KUGETI(RRECL)
      CALL KUGETC(RFORM,NR)
      CALL KUGETI(LRECL)
      CALL KUGETC(LFORM,NL)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
      CALL XZGETF(LOCAL,REMOTE,LRECL,LFORM,RRECL,RFORM,CHOPT,IRC)
99    END
 
