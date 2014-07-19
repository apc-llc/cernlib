*
* $Id: putd.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: putd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE PUTD
*
*         Action routine for PUTD command
*
#include "cspack/czsock.inc"
      CHARACTER*80 REMOTE,LOCAL
      CHARACTER*8  CHOPT
*
#include "cspack/czopen.inc"
      CALL KUGETS(LOCAL,NCHL)
      CALL KUGETS(REMOTE,NCHR)
      IF(NCHR.LE.0)REMOTE=LOCAL
      IF(REMOTE(1:NCHR).EQ.'=') REMOTE=LOCAL
      NCHR = LENOCC(REMOTE)
      CALL KUGETI(LRECL)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
      CALL XZPUTD(LOCAL,REMOTE,LRECL,CHOPT,IRC)
99    END
 
