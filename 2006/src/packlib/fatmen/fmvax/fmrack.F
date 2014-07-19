*
* $Id: fmrack.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmrack.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRACK(LUNNET,CHACK,CHJOB,IRC)
*CMZ :          19/02/92  12.57.00  by  Jamie Shiers
*-- Author :    Jamie Shiers   19/02/92
      CHARACTER*(*) CHACK,CHJOB
#include "fatmen/fatbug.inc"
#include "fatmen/farstg.inc"
*
*     Read acknowledgement back from server
*
      IRC    = 0
*
*     Give server a chance to do something
*
      CALL SLEEPF(IWTACK)
 
      IRETRY = 0
   10 CONTINUE
      IRETRY = IRETRY + 1
      READ(LUNNET,'(A)',END=20,ERR=30,IOSTAT=IRC) CHACK
      READ(LUNNET,'(A)',END=20,ERR=30,IOSTAT=IRC) CHJOB
      RETURN
 
   20 CONTINUE
      IF(IRETRY.LT.MAXACK) THEN
         REWIND(LUNNET)
         IF(IDEBFA.GE.0) PRINT 9001,IWTACK,IRETRY
 9001 FORMAT(' FMRACK. no reply from server - waiting ',I6,
     +       ' seconds before retry # ',I6)
         CALL SLEEPF(IWTACK)
         GOTO 10
      ELSE
         IRC = -1
         CHACK = 'no reply from server'
         IF(IDEBFA.GE.0) PRINT 9002,MAXACK
 9002 FORMAT(' FMRACK. Still no reply from server -',
     +       ' maximum retry count (',I6,') reached')
      ENDIF
 
      RETURN
 
   30 CONTINUE
*
*     Error reading reply from server
*
      WRITE(CHACK,9003) IRC
 9003 FORMAT('Error ',I10,' reading reply from STGSERV')
 
      END
