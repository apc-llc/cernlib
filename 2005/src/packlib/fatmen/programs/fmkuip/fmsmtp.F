*
* $Id: fmsmtp.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmsmtp.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSMTP
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatinit.inc"
 
      IF(NUMMTP.EQ.0) THEN
         PRINT *,'FMSMTP. no media types defined. ',
     +           'Checks on media type will be skipped'
      ELSE
         DO 10 I=1,NUMMTP
            PRINT 9001,I,MFMMTP(I)
9001  FORMAT(' FMSMTP. media type # ',I2.2,' = ',I6.6)
   10 CONTINUE
      ENDIF
 
      END
