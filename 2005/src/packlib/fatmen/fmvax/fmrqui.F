*
* $Id: fmrqui.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmrqui.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRQUI(LUNNET,CHNODE,CHENT,IRC)
*CMZ :          17/02/92  16.34.31  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/02/92
      CHARACTER*(*) CHNODE,CHENT
      CHARACTER*8   CHHOST
      CHARACTER*255 CHNAME,CHTASK,CHMESS,CHLINE,CHJOB
#include "fatmen/farstg.inc"
#include "fatmen/fatbug.inc"
      INCLUDE '($FORDEF)'
      PARAMETER (IO_OK=0)
 
      IRC   = 0
      LNODE = LENOCC(CHNODE)
*
*     Open communication with remote server
*
      CHHOST = CHNODE(1:LNODE)
      CHTASK = CHHOST(1:LNODE) // '::"TASK=STGSERV"'
      LTASK  = LENOCC(CHTASK)
 
      IWAIT  = IWTPND
   10 CONTINUE
*
*     Restart server (maybe DECnet has kept it alive for us...)
*
      IRETRY = 0
   20 CONTINUE
      OPEN(UNIT=LUNNET,FILE=CHTASK(1:LTASK),STATUS='NEW',ERR=30,
     +FORM='FORMATTED',RECL=255)
*
*     Send command
*
      WRITE(LUNNET,'(A)',ERR=40) 'QUERY'
      WRITE(LUNNET,'(A)',ERR=40) CHENT
*
*     Read back acknowledgement
*
      CALL FMRACK(LUNNET,CHMESS,CHJOB,IRC)
 
      IF(IDEBFA.GE.0.OR.IRC.NE.0) THEN
         LMESS = LENOCC(CHMESS)
         CALL DATIME(ID,IT)
      ENDIF
 
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9002) CHMESS(1:LMESS)
 9002 FORMAT(' FMRQUI. error during remote job inquiry - ',A)
         GOTO 50
      ELSE
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9003) ID,IT,CHMESS(1:LMESS)
 9003 FORMAT(' FMRQUI. job status at ',I6,1X,I4,' = ',A)
      ENDIF
*
*     Job pending, executing or disappeared?
*
      IF(INDEX(CHMESS,'PENDING').NE.0) THEN
         CLOSE(LUNNET)
         GOTO 50
      ELSEIF(INDEX(CHMESS,'EXECUTING').NE.0) THEN
         CLOSE(LUNNET)
         IWAIT = IWTEXE
         GOTO 50
      ELSEIF(INDEX(CHMESS,'COMPLETED').NE.0) THEN
         GOTO 50
      ELSE
         CLOSE(LUNNET)
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9001) IWAIT
 9001    FORMAT(' FMRQUI. please wait - next job status in ',I10,
     +       ' seconds')
         CALL SLEEPF(IWAIT)
         GOTO 10
      ENDIF
 
   30 CONTINUE
      IRETRY = IRETRY + 1
      IF(IRETRY.LE.MAXNET) THEN
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9004) IWTNET
 9004 FORMAT(' FMRQUI. error starting server - retry in ',I10,
     +       ' seconds')
         CALL SLEEPF(IWTNET)
         GOTO 20
      ENDIF
 
      IF(IDEBFA.GE.-3) WRITE(LPRTFA,9005) MAXNET
 9005 FORMAT(' FMRQUI. maximum number of network retries exceeded.',
     +       ' Retry count = ',I10)
      IRC = -2
      GOTO 60
 
   40 CONTINUE
      IRETRY = IRETRY + 1
      IF(IRETRY.LE.MAXNET) THEN
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9006) IWTNET
 9006 FORMAT(' FMRQUI. error writing to server - retry in ',I10,
     +       ' seconds')
         CALL SLEEPF(IWTNET)
         GOTO 20
      ENDIF
 
      IF(IDEBFA.GE.-3) WRITE(LPRTFA,9005) MAXNET
      IRC = -2
      GOTO 60
 
   50 CONTINUE
 
      CLOSE(LUNNET)
 
   60 CONTINUE
 
      END
