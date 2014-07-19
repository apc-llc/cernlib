*
* $Id: fmscpl.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmscpl.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSCPL
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fmdrep.inc"
#include "fatmen/fatinit.inc"
 
      IF(NUMCPL.EQ.0) THEN
         PRINT *,'FMSCPL. no data representations defined. ',
     +           'Checks on data repr. will be skipped'
      ELSE
         DO 10 I=1,NUMCPL
            PRINT 9001,I,MFMCPL(I),CHDREP(MFMCPL(I))
9001  FORMAT(' FMSCPL. data repr. # ',I2.2,' = ',I6.6,' = ',A30)
   10 CONTINUE
      ENDIF
 
      END
