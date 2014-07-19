*
* $Id: fmrstg.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmrstg.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRSTG(CHNODE,CHFILE,CHTYPE,CHCOMM,ISIZE,CHOPT,IRC)
      CHARACTER*(*) CHNODE,CHFILE,CHTYPE,CHCOMM
      CHARACTER*8   CHHOST
      CHARACTER*255 CHNAME,CHTASK,CHENT,CHMESS,CHLINE,CHJOB,CHFULL
      INCLUDE '($FORDEF)'
      PARAMETER (IO_OK=0)
      LOGICAL       QUEUED
#include "fatmen/fmqued.inc"
#include "fatmen/slate.inc"
#include "fatmen/farstg.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatopts.inc"
 
      IRC   = 0
      QUEUED = .FALSE.
      LNODE = LENOCC(CHNODE)
      LFILE = LENOCC(CHFILE)
      LTYPE = LENOCC(CHTYPE)
      LCOMM = LENOCC(CHCOMM)
 
      IF(IDEBFA.GE.1) PRINT *,'FMRSTG. enter for ',
     +   'node,file,type,comm = ',
     +   CHNODE(1:LNODE),' ',CHFILE(1:LFILE),' ',CHTYPE(1:LTYPE),
     +   CHCOMM(1:LCOMM)
 
      LDOT   = INDEX(CHFILE(1:LFILE),'.')
      CHNAME = CHFILE(1:LFILE)
      CHNAME(LDOT:LDOT) = '_'
*
*     FULLTAPE list
*
      CALL FMGTLG('STAGE_FULLTAPE',CHFULL,'LNM$SYSTEM',ISTAT)
      LFULL = IS(1)
*
*     Get unit for network connection
*
      ISTAT = LIB$GET_LUN(LUNNET)
#include "fatmen/fatvaxrc.inc"
*
*     Open communication with remote server
*
      CHHOST = CHNODE(1:LNODE)
      CHTASK = CHHOST(1:LNODE) // '::"TASK=STGSERV"'
      LTASK  = LENOCC(CHTASK)
 
      IRETRY = 0
   10 CONTINUE
      OPEN(UNIT=LUNNET,FILE=CHTASK(1:LTASK),STATUS='NEW',ERR=100,
     +FORM='FORMATTED',RECL=255)
*
*     First check whether tape is in use
*
      WRITE(LUNNET,'(A)') 'LOOK'
*
*     Send file name, generic device type and stage command
*
      WRITE(LUNNET,'(A)') CHNAME(1:LDOT-1)
      WRITE(LUNNET,'(A)') CHTYPE(1:LTYPE)
*
*     Read back job entry number. 0 = no such job
*
      CALL FMRACK(LUNNET,CHENT,CHJOB,IRC)
      LCHENT = LENOCC(CHENT)
 
      IF(IDEBFA.GE.3) PRINT *,'FMRSTG. reply from stage server = ',
     +   CHENT(1:LCHENT)
 
      JX = ICDECI(CHENT,1,LCHENT)
 
      IF(JX.GT.0) THEN
         IF(IDEBFA.GE.0) THEN
            CALL DATIME(ID,IT)
            PRINT *,'FMRSTG. tape in use at ',ID,IT,
     +              ' on server cluster'
            PRINT *,'FMRSTG. waiting for job ',CHENT(1:LCHENT),
     +      ' to release tape'
         ENDIF
         CLOSE(LUNNET)
         NCNAME=INDEX(CHNAME(LDOT+1:LFILE),'_')+LDOT-1
         NJNAME=INDEX(CHJOB(LDOT+1:LFILE),'_')+LDOT-1
         IF(CHNAME(LDOT+1:NCNAME).EQ.
     +      CHJOB(LDOT+1:NJNAME)) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRSTG. The other job is staging ',
     +      'the same file !'
            IF(IOPTQ.NE.0) GOTO 140
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMRSTG. The other job is staging ',
     +      'another file.'
            IF(IOPTQ.NE.0.AND.
     +         INDEX(CHFULL(1:LFULL),CHTYPE(1:LTYPE)).NE.0) GOTO 140
*
*     If we are in server mode, queue request
*
            IF(LQUED.GT.0.AND..NOT.QUEUED) THEN
C
C ****  check if the job is a real staging job
C
               IF ( CHJOB(LDOT+1:NJNAME).NE.'XX') THEN
                  QUEUED = .TRUE.
                  CALL FMQSTG(CHFILE,IRC)
C
C ****  check if the job is still there
C
                  CALL SLEEPF(IWTACK)
                  IRETRY = 0
   20             CONTINUE
                  OPEN(UNIT=LUNNET,FILE=CHTASK(1:LTASK), STATUS='NEW',
     +            ERR=30 , FORM='FORMATTED',RECL=255)
                  GOTO 40
*
*       Error starter server - retry
*
   30             CONTINUE
                  IRETRY = IRETRY + 1
                  IF(IRETRY.LE.MAXNET) THEN
                     IF(IDEBFA.GE.1) PRINT *,'FMRSTG. error starting '
     +               //'server - ', 'retry in ',IWTNET,' seconds'
                     CALL SLEEPF(IWTNET)
                     GOTO 20
                  ENDIF
 
                  IF(IDEBFA.GE.-3) PRINT *,'FMRSTG. maximum number of '
     +            //'network ', 'retries exceeded. Retry count = ',
     +            MAXNET
                  IRC = -2
                  GOTO 140
 
   40             CONTINUE
                  WRITE(LUNNET,'(A)') 'LOOK'
*
*     Send file name, generic device type and stage command
*
                  WRITE(LUNNET,'(A)') CHNAME(1:LDOT-1)
                  WRITE(LUNNET,'(A)') CHTYPE(1:LTYPE)
*
*     Read back job entry number. 0 = no such job
*
                  CALL FMRACK(LUNNET,CHENT,CHJOB,IRC)
                  LCHENT = LENOCC(CHENT)
 
                  IF(IDEBFA.GE.3)
     +              PRINT *,'FMRSTG. reply from stage server = ',
     +              CHENT(1:LCHENT)
 
                  JX = ICDECI(CHENT,1,LCHENT)
                  IF (JX.LE.0) THEN
                     CLOSE(LUNNET)
                     GOTO 50
                  ELSE
                     CLOSE(LUNNET)
                  ENDIF
 
               ENDIF
 
            ENDIF
 
            IF(IOPTQ.NE.0) GOTO 140
 
         ENDIF
 
         CALL FMRQUI(LUNNET,CHNODE(1:LNODE),CHENT(1:LCHENT),
     +               CHFILE(1:LFILE),IRC)
         IWAIT  = IWTPND
         IF(IDEBFA.GE.0) PRINT *,'FMRSTG. please wait - next job ',
     +   'status in ',IWAIT,' seconds'
         CALL SLEEPF(IWAIT)
*
*     Now check if the file we wanted has been staged
*
         CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
         IF(IFOUND.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRSTG. The file we want ',
     +      'is on now on disk!'
            GOTO 140
         ELSE
            GOTO 10
         ENDIF
      ELSE
         CLOSE(LUNNET)
      ENDIF
      IRETRY = 0
   50 CONTINUE
      OPEN(UNIT=LUNNET,FILE=CHTASK(1:LTASK),STATUS='NEW',ERR=110,
     +FORM='FORMATTED',RECL=255)
*
*     Send command
*
      WRITE(LUNNET,'(A)') 'STAGE'
*
*     Send file name, generic device type and stage command
*
      WRITE(LUNNET,'(A)') CHNAME(1:LFILE)
      WRITE(LUNNET,'(A)') CHTYPE(1:LTYPE)
      WRITE(LUNNET,'(A)') CHCOMM(1:LCOMM)
*
*     Wait for job submission - read back job entry number
*
      CALL FMRACK(LUNNET,CHENT,CHJOB,IRC)
*
*     Check if returned message is numeric - if so it is job entry number
*
      LCHENT = LENOCC(CHENT)
 
      IF(IDEBFA.GE.3) PRINT *,'FMRSTG. reply from stage server = ',
     +   CHENT(1:LCHENT)
 
      JX     = ICNUM(CHENT,1,LCHENT)
      IF(JX.LE.LCHENT) IRC = -1
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMRSTG. error during remote ',
     +      'job submission - ',CHENT(1:LCHENT)
         GOTO 130
      ELSE
         IF(IDEBFA.GE.0) THEN
            CALL DATIME(ID,IT)
            PRINT *,'FMRSTG. stage job successfully ',
     +      'submitted to node ',CHNODE(1:LNODE),'. Job entry ',
     +      'number = ',CHENT(1:LCHENT)
            PRINT *,'FMRSTG. The time is now ',ID,IT
         ENDIF
      ENDIF
      IF(IOPTQ.NE.0) GOTO 140
*
*     Now loop until job is completed
*
      CLOSE(LUNNET)
      IWAIT  = IWTPND
   60 CONTINUE
      IF(IDEBFA.GE.0) PRINT *,'FMRSTG. please wait - next job ',
     +   'status in ',IWAIT,' seconds'
      CALL SLEEPF(IWAIT)
*
*     Restart server (maybe DECnet has kept it alive for us...)
*
      IRETRY = 0
   70 CONTINUE
 
      OPEN(UNIT=LUNNET,FILE=CHTASK(1:LTASK),STATUS='NEW',ERR=120,
     +FORM='FORMATTED',RECL=255)
*
*     Send command
*
      WRITE(LUNNET,'(A)') 'QUERY'
      WRITE(LUNNET,'(A)') CHENT
*
*     Read back acknowledgement
*
      CALL FMRACK(LUNNET,CHMESS,CHJOB,IRC)
 
      IF(IDEBFA.GE.0.OR.IRC.NE.0) THEN
         LMESS = LENOCC(CHMESS)
         CALL DATIME(ID,IT)
      ENDIF
 
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMRSTG. error during remote ',
     +      'job inquiry - ',CHMESS(1:LMESS)
         GOTO 130
      ELSE
         IF(IDEBFA.GE.0) PRINT *,'FMRSTG. job status at ',ID,IT,' = ',
     +      CHMESS(1:LMESS)
      ENDIF
*
*     Job pending, executing or disappeared?
*
      IF(INDEX(CHMESS,'PENDING').NE.0) THEN
         CLOSE(LUNNET)
*
*     Check if the file has been staged successfully
*     (maybe by someone else)
*
         CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
         IF(IFOUND.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The file we want ',
     +         'is on now on disk!'
            GOTO 140
         ENDIF
         GOTO 60
      ELSEIF(INDEX(CHMESS,'EXECUTING').NE.0) THEN
         CLOSE(LUNNET)
*
*     Check if the file has been staged successfully
*     (maybe by someone else)
*
         CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
         IF(IFOUND.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The file we want ',
     +         'is on now on disk!'
            GOTO 140
         ENDIF
         IWAIT = IWTEXE
         GOTO 60
      ELSEIF(INDEX(CHMESS,'COMPLETED').NE.0) THEN
*
*     Now check stage log file
*
         CLOSE(LUNNET)
 
         ISTAT = LIB$GET_LUN(LUNSTG)
#include "fatmen/fatvaxrc.inc"
 
         OPEN(LUNSTG,FILE='STAGE_LOGS:'//
     +        CHNAME(1:LFILE)//'.LOG',FORM='FORMATTED',
     +        STATUS='OLD',READONLY,IOSTAT=ISTAT)
         IF(ISTAT.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRSTG. cannot open ',
     +         'stage log file'
            CALL LIB$FREE_LUN(LUNSTG)
            GOTO 140
         ENDIF
 
         IF(IDEBFA.GE.0) THEN
            PRINT *
            PRINT *,'FMRSTG. stage log file follows...'
            PRINT *
         ENDIF
 
         IFOUND = 0
 
   80    CONTINUE
         READ(LUNSTG,'(A)',END=90) CHLINE
         LLINE = LENOCC(CHLINE)
 
         IF(IFOUND.EQ.0) THEN
            IF(INDEX(CHLINE(1:LLINE),CHCOMM(1:LCOMM)).EQ.0) GOTO 80
            IFOUND = 1
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,CHLINE(1:LLINE)
 
         IF(IFOUND.EQ.1) THEN
 
            IF(INDEX(CHLINE(1:LLINE),'STAGE-I-LOGNAME').GT.0) THEN
               IFOUND = 2
            ENDIF
 
         ENDIF
 
         GOTO 80
 
   90    CONTINUE
         CLOSE(LUNSTG)
 
         CALL LIB$FREE_LUN(LUNSTG)
 
         IF(IDEBFA.GE.0) THEN
            PRINT *
            PRINT *,'FMRSTG. end of stage log file'
            PRINT *
         ENDIF
*
*     Change this to an error if confident...
*
         IF(IFOUND.NE.2) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMRSTG. warning - could not ',
     +         'find completion message in stage log'
            IRC = -2
         ENDIF
 
         GOTO 140
 
      ENDIF
 
  100 CONTINUE
      IRETRY = IRETRY + 1
      IF(IRETRY.LE.MAXNET) THEN
         IF(IDEBFA.GE.1) PRINT *,'FMRSTG. error starting server - ',
     +      'retry in ',IWTNET,' seconds'
         CALL SLEEPF(IWTNET)
         GOTO 10
      ENDIF
 
      IF(IDEBFA.GE.-3) PRINT *,'FMRSTG. maximum number of network ',
     +   'retries exceeded. Retry count = ',MAXNET
      IRC = -2
      GOTO 140
 
  110 CONTINUE
      IRETRY = IRETRY + 1
      IF(IRETRY.LE.MAXNET) THEN
         IF(IDEBFA.GE.1) PRINT *,'FMRSTG. error starting server - ',
     +      'retry in ',IWTNET,' seconds'
         CALL SLEEPF(IWTNET)
         GOTO 50
      ENDIF
 
      IF(IDEBFA.GE.-3) PRINT *,'FMRSTG. maximum number of network ',
     +   'retries exceeded. Retry count = ',MAXNET
      IRC = -2
      GOTO 140
 
  120 CONTINUE
      IRETRY = IRETRY + 1
      IF(IRETRY.LE.MAXNET) THEN
         IF(IDEBFA.GE.1) PRINT *,'FMRSTG. error starting server - ',
     +      'retry in ',IWTNET,' seconds'
         CALL SLEEPF(IWTNET)
         GOTO 70
      ENDIF
 
      IF(IDEBFA.GE.-3) PRINT *,'FMRSTG. maximum number of network ',
     +   'retries exceeded. Retry count = ',MAXNET
      IRC = -2
      GOTO 140
 
  130 CONTINUE
 
      CLOSE(LUNNET)
 
  140 CONTINUE
*
*     Free unit for network connection
*
      ISTAT = LIB$FREE_LUN(LUNNET)
#include "fatmen/fatvaxrc.inc"
 
      END
