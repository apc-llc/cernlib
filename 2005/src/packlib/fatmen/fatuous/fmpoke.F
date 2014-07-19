*
* $Id: fmpoke.F,v 1.1.1.1 1996/03/07 15:18:04 mclareni Exp $
*
* $Log: fmpoke.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:04  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPOKE(GENAME,IVECT,CHOPT,IRC)
*CMZ :          11/12/90  15.28.48  by  Jamie Shiers
*-- Author :    Jamie Shiers   11/12/90
      CHARACTER*(*) GENAME
#include "fatmen/fatbug.inc"
#include "fatmen/fat.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      DIMENSION     IVECT(NWDSFA)
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatopts.inc"
      IRC = 0
      LGN = LENOCC(GENAME)
      IF(IOPTN.EQ.0) CALL UCOPY(IVECT(1),IQ(LADDBK+1),NWDSFA)
      IF(IOPTP.NE.0) THEN
         CALL FMPUT(GENAME(1:LGN),LADDBK,IRC)
      ELSEIF(IOPTM.NE.0) THEN
         CALL FMMOD(GENAME(1:LGN),LADDBK,0,IRC)
      ELSEIF(IOPTR.NE.0) THEN
         CALL FMMOD(GENAME(1:LGN),LADDBK,1,IRC)
      ENDIF
   99 END
