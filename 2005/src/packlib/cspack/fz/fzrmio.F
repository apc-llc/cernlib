*
* $Id: fzrmio.F,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
*
* $Log: fzrmio.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE FZRMIO(IBUF,IOWAY)
      DIMENSION IBUF(8192)
      PARAMETER (NREC=0)
#include "cspack/zmach.inc"
#include "cspack/quest.inc"
      CHARACTER*6  CHWAY
 
      IRC = 0
      LUN = IQUEST(1)
      IF(IOWAY.EQ.0) THEN
         CALL XZREAD(LUN,IBUF,NREC,IQUEST(2)*IQCHAW,NGOT,' ',IRC)
         ELSEIF(IOWAY.EQ.1) THEN
         CALL XZRITE(LUN,IBUF,NREC,IQUEST(2)*IQCHAW,' ',IRC)
         ELSE
         WRITE(CHWAY,'(I6)') IOWAY
         CALL ZFATAM('Invalid value for IOWAY in FZRMIO - '//CHWAY)
         ENDIF
 
      IQUEST(1) = IRC
 
      END
 
