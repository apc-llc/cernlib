*
* $Id: fmcstg.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmcstg.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCSTG(CHFILE,CHTYPE,CHCOMM,ISIZE,CHOPT,IRC)
*CMZ :          17/02/92  16.34.31  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/02/92
      CHARACTER*(*) CHFILE,CHTYPE,CHCOMM
      CHARACTER*255 CHNAME,CHMESS,CHLINE,JOBNAM,CHFULL
      LOGICAL       MYFILE,QUEUED
#include "fatmen/fmqued.inc"
#include "fatmen/farstg.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatopts.inc"
 
      IRC    = 0
      MYFILE = .FALSE.
      QUEUED = .FALSE.
      LFILE  = LENOCC(CHFILE)
      LTYPE  = LENOCC(CHTYPE)
      LCOMM  = LENOCC(CHCOMM)
 
      IF(IDEBFA.GE.3) PRINT *,'FMCSTG. enter for ',
     +   'file,type,comm = ',
     +   CHTYPE(1:LTYPE),' ',CHFILE(1:LFILE),CHCOMM(1:LCOMM)
 
      LDOT   = INDEX(CHFILE(1:LFILE),'.')
      CHNAME = CHFILE(1:LFILE)
      CHNAME(LDOT:LDOT) = '_'
*
*     FULLTAPE list
*
      CALL FMGTLG('STAGE_FULLTAPE',CHFULL,'LNM$SYSTEM',ISTAT)
      LFULL = IS(1)
*
*     Look if there already is a job accessing the tape that
*     we want in any of the staging queues
*
      JOBNAM=' '
   10 CONTINUE
      CALL FMLOOK(CHNAME(1:LDOT-1),'*STAGE*',JOBNAM,JOBNUM)
 
      IF(JOBNUM.GT.0) THEN
 
         CALL DATIME(ID,IT)
 
         IF(IDEBFA.GE.0) PRINT *,'FMCSTG. tape used by job ',
     +      JOBNAM(1:LENOCC(JOBNAM))
         IF(IDEBFA.GE.0) PRINT *,' the time now is ',ID,IT
         IF(IOPTQ.EQ.0.AND.LQUED.EQ.0.AND.IDEBFA.GE.0)
     +      PRINT *,' waiting until tape is free'
         NCNAME=INDEX(CHNAME(LDOT+1:LFILE),'_')+LDOT-1
         NJNAME=INDEX(JOBNAM(LDOT+1:LFILE),'_')+LDOT-1
         IF(CHNAME(LDOT+1:NCNAME).EQ.
     +      JOBNAM(LDOT+1:NJNAME)) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The other job is staging ',
     +      'the same file !'
            IF(IOPTQ.NE.0) RETURN
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The other job is staging ',
     +      'another file.'
            IF(IOPTQ.NE.0.AND.
     +         INDEX(CHFULL(1:LFULL),CHTYPE(1:LTYPE)).NE.0) RETURN
*
*     If we are in server mode, queue request
*
            IF(LQUED.GT.0.AND..NOT.QUEUED) THEN
C
C ****  check if the job is a real staging job
C
               IF (JOBNAM(LDOT+1:NJNAME).NE.'XX') THEN
                  QUEUED = .TRUE.
                  CALL FMQSTG(CHFILE,IRC)
C
C ****  check if the job is still there
C
                  CALL SLEEPF(IWTACK)
                  JOBNAM=' '
                  CALL FMLOOK(CHNAME(1:LDOT-1),'*STAGE*',JOBNAM,JOBNUM)
                  IF (JOBNUM.EQ.0) THEN
                     GOTO 30
                  ELSEIF(JOBNUM.LT.0) THEN
                     IF(IDEBFA.GE.-3) PRINT *, 'FMCSTG. FMLOOK failed'
                  ENDIF
 
               ENDIF
 
            ENDIF
 
            IF(IOPTQ.NE.0) RETURN
         ENDIF
 
         IWAIT  = IWTPND
   20    CONTINUE
         IF(IDEBFA.GE.0) PRINT *,'FMCSTG. please wait - next job ',
     +      'status in ',IWAIT,' seconds'
         CALL SLEEPF(IWAIT)
 
         CALL FASTGQ(JOBNUM,CHMESS,IRC)
         LMESS = LENOCC(CHMESS)
 
         CALL DATIME(ID,IT)
 
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. unexpected status from ',
     +      'job inquiry - ',IRC
            GOTO 70
         ELSE
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. status at ',ID,IT,' = ',
     +      CHMESS(1:LMESS)
         ENDIF
*
*     Job pending, executing or disappeared?
*
         IF(INDEX(CHMESS(1:LMESS),'PENDING').NE.0) THEN
*
*     Check whether they already were kind enough to stage the file
*     that we want
*
            CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
            IF(IFOUND.EQ.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The file we want ',
     +          'is on now on disk!'
               GOTO 70
            ENDIF
            GOTO 20
         ELSEIF(INDEX(CHMESS(1:LMESS),'EXECUTING').NE.0) THEN
            IWAIT = IWTEXE
*
*     Check whether they already were kind enough to stage the file
*     that we want
*
            CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
            IF(IFOUND.EQ.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The file we want ',
     +          'is on now on disk!'
               GOTO 70
            ENDIF
            GOTO 20
         ELSEIF(INDEX(CHMESS(1:LMESS),'COMPLETED').NE.0) THEN
            IF(IDEBFA.GE.1) PRINT *,'FMCSTG. job has completed - ',
     +      'we can continue'
         ENDIF
*
*     Check whether they were kind enough to stage the file
*     that we want
*
         CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
         IF(IFOUND.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. and the file we want ',
     +      'is on now on disk!'
            GOTO 70
         ELSE
            GOTO 10
         ENDIF
      ELSEIF (JOBNUM.LT.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMCSTG. FMLOOK failed'
      ENDIF
 
   30 CONTINUE
*
*     Submit job
*
      CHMESS=' '
      CALL FASTGS(CHNAME(1:LFILE),CHTYPE(1:LTYPE),CHCOMM(1:LCOMM),
     +   JOBNUM,CHMESS,IRC)
 
      LMESS = LENOCC(CHMESS)
 
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMCSTG. error submitting ',
     +      'stage job - ',CHMESS(1:LMESS)
         RETURN
      ENDIF
 
      CALL DATIME(ID,IT)
 
      IF(IDEBFA.GE.0) PRINT *,'FMCSTG. submit status at ',
     +   ID,IT,' = ',CHMESS(1:LMESS)
      IF(IOPTQ.NE.0) RETURN
*
*     Check batch queue until job is finished
*
      IWAIT  = IWTPND
   40 CONTINUE
      IF(IDEBFA.GE.0) PRINT *,'FMCSTG. please wait - next job ',
     +   'status in ',IWAIT,' seconds'
      CALL SLEEPF(IWAIT)
 
      CALL FASTGQ(JOBNUM,CHMESS,IRC)
 
      LMESS = LENOCC(CHMESS)
 
      CALL DATIME(ID,IT)
 
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMCSTG. unexpected status from ',
     +      'job inquiry - ',IRC
         GOTO 70
      ELSE
         IF(IDEBFA.GE.0) PRINT *,'FMCSTG. status at ',ID,IT,' = ',
     +      CHMESS(1:LMESS)
      ENDIF
*
*     Job pending, executing or disappeared?
*
      IF(INDEX(CHMESS(1:LMESS),'PENDING').NE.0) THEN
*
*     First check if the file has been staged successfully
*     (maybe by someone else)
*
         CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
 
         IF(IFOUND.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The file we want ',
     +         'is on now on disk!'
            GOTO 70
         ENDIF
         GOTO 40
      ELSEIF(INDEX(CHMESS(1:LMESS),'EXECUTING').NE.0) THEN
         IWAIT = IWTEXE
*
*     First check if the file has been staged successfully
*     (maybe by someone else)
*
         CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,IFOUND)
 
         IF(IFOUND.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. The file we want ',
     +         'is on now on disk!'
            GOTO 70
         ENDIF
         GOTO 40
 
      ELSEIF(INDEX(CHMESS(1:LMESS),'COMPLETED').NE.0) THEN
*
*     Now check stage log file
*
         ISTAT = LIB$GET_LUN(LUNSTG)
#include "fatmen/fatvaxrc.inc"
         OPEN(LUNSTG,FILE='STAGE_LOGS:'//
     +        CHNAME(1:LFILE)//'.LOG',FORM='FORMATTED',
     +        STATUS='OLD',READONLY,IOSTAT=ISTAT)
         IF(ISTAT.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. cannot open ',
     +         'stage log file'
            CALL LIB$FREE_LUN(LUNSTG)
            GOTO 70
         ENDIF
 
         IF(IDEBFA.GE.0) THEN
            PRINT *
            PRINT *,'FMCSTG. stage log file follows...'
            PRINT *
         ENDIF
 
         IFOUND = 0
 
   50    CONTINUE
         READ(LUNSTG,'(A)',END=60) CHLINE
         LLINE = LENOCC(CHLINE)
 
         IF(IFOUND.EQ.0) THEN
            IF(INDEX(CHLINE(1:LLINE),CHCOMM(1:LCOMM)).EQ.0) GOTO 50
            IFOUND = 1
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,CHLINE(1:LLINE)
 
         IF(IFOUND.EQ.1) THEN
 
            IF(INDEX(CHLINE(1:LLINE),'STAGE-I-LOGNAME').GT.0) THEN
               IFOUND = 2
            ENDIF
 
         ENDIF
 
         GOTO 50
 
   60    CONTINUE
         CLOSE(LUNSTG)
 
         CALL LIB$FREE_LUN(LUNSTG)
 
         IF(IDEBFA.GE.0) THEN
            PRINT *
            PRINT *,'FMCSTG. end of stage log file'
            PRINT *
         ENDIF
*
*     Change this to an error if confident...
*
         IF(IFOUND.NE.2) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMCSTG. warning - could not ',
     +         'find completion message in stage log'
           IRC = -2
 
         ENDIF
 
         GOTO 70
 
      ENDIF
 
      IRC = 1
 
   70 RETURN
 
      END
