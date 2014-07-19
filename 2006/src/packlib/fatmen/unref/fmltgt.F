*
* $Id: fmltgt.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmltgt.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLTGT(MATCH,SUBDIR,NSUB,IC)
      CHARACTER*(*) SUBDIR(NSUB)
      CHARACTER*20  CHDIR(1000)
      CHARACTER*(*) MATCH
      CHARACTER*20  XMATCH
*
*     First, find all subdirs which match, replacing < & > by *
*
      LMATCH = LENOCC(MATCH)
      XMATCH = MATCH(1:LMATCH)
      CALL CTRANS('<','*',XMATCH,1,LMATCH)
      CALL CTRANS('>','*',XMATCH,1,LMATCH)
      NPASS  = 0
      DO 10 I=1,NSUB
      CALL FMATCH(SUBDIR(I),XMATCH(1:LMATCH),IRC)
      IF(IRC.NE.0) GOTO 10
      NPASS  = NPASS + 1
      CHDIR(NPASS) = SUBDIR(I)
10    CONTINUE
 
      END
 
