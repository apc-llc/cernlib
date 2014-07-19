*
* $Id: fmsloc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmsloc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSLOC
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatinit.inc"
 
      IF(NUMLOC.EQ.0) THEN
         PRINT *,'FMSLOC. no location codes defined. ',
     +           'Checks on location code will be skipped'
      ELSE
         DO 10 I=1,NUMLOC
            PRINT 9001,I,MFMLOC(I)
9001  FORMAT(' FMSLOC. location code # ',I2.2,' = ',I6.6)
   10 CONTINUE
      ENDIF
 
      END
