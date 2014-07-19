*
* $Id: szrite.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szrite.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZRITE(LUN,IBUFF,NREC,NWRIT,CHOPT,IRC)
#include "cspack/zmach.inc"
#include "cspack/czsock.inc"
      INTEGER IBUFF(NWRIT/IQCHAW)
#include "cspack/czopts.inc"
      IRC = 0
 
      IF(NREC.EQ.0) THEN
         WRITE(LUN,ERR=99) IBUFF
         ELSEIF(IOPTV.NE.0) THEN
         NWORDS = NWRIT/IQCHAW
         WRITE(LUN,ERR=99,IOSTAT=IRC) NWORDS,(IBUFF(I),I=1,NWORDS)
         ELSE
         WRITE(UNIT=LUN,REC=NREC,ERR=99,IOSTAT=IRC)IBUFF
         ENDIF
 
      RETURN
 
99    END
 
