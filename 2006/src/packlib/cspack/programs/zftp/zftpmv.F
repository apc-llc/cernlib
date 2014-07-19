*
* $Id: zftpmv.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zftpmv.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTPMV
#include "cspack/czsock.inc"
#include "cspack/hcmail.inc"
#include "cspack/quest.inc"
      CHARACTER*80 CHMESS
      CHARACTER*80 LOCAL,REMOTE
      CHARACTER*8  CHOPT
*
#include "cspack/czopen.inc"
      CALL KUGETS(LOCAL,LSRC)
      CALL KUGETS(REMOTE,LDST)
      CALL KUGETC(CHOPT,LOPT)
#include "cspack/czfold.inc"
      CALL XZMV(LOCAL(1:LSRC),REMOTE(1:LDST),CHOPT(1:LOPT),IRC)
  99  END
 
