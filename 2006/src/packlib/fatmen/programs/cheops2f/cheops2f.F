*
* $Id: cheops2f.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: cheops2f.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM CHEOPS2F
*
*     Program to read CHEOPS .rqid and .rep files and update
*     FATMEN catalogue accordingly
*
#include "fatmen/slate.inc"
#include "zebra/quest.inc"
#include "fatmen/cheops.inc"
      INTEGER SYSTEMF
      LOGICAL EXIST
      CHARACTER*8   CHWAKE
      CHARACTER*20  CHREQ
      CHARACTER*255 CHDIR,CHFILE,CHNAME,CHSTR
      CHARACTER*255 CHLINE,CHSRCE,CHDEST
      PARAMETER (LURCOR=250000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON/USRLNK/LSRCBK,LDSTBK
      INTEGER FMHOST,FMUSER,FAFNDF
      CHARACTER*12   CHUSER,CHSYS,CHLOG
      CHARACTER*80   CHCOMM
*
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
      ILOG   = 0
      LUNRZ  = 1
      LUFZFA = 2
      LUNREQ = 3
      LPRTFA = 6
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
 
      CALL MZLOGL(IXSTOR,-3)
*
* *** Define user division and link area like:
*
      CALL MZDIV (IXSTOR,IXDIV,'USERS',50000,LURCOR, 'L')
      CALL MZLINK(IXSTOR,'/USRLNK/',LSRCBK,LDSTBK,LSRCBK)
*
*     I/O descriptor for FATMEN bank
*
      CALL MZFORM('DSFA','66H 3I 11H 8I 1H 2I 2H 3I 8H 21I 20H',IODSFA)
 
      CALL GETENVF('FMWAKEUP',CHWAKE)
      LCHW  = IS(1)
      IF(LCHW.GT.0) THEN
         IC = 0
      ELSE
         IC = -1
      ENDIF
 
      IF(IC.EQ.0) THEN
         ISLEEP = ICDECI(CHWAKE,1,8)
      ENDIF
      IF(ISLEEP.EQ.0) THEN
         ISLEEP = 60
         WRITE(CHWAKE,'(I8.8)') ISLEEP
      ENDIF
*
*     Print FATMEN version number
*
      CALL FMVERS
*
      PRINT *,'CHEOPS. wakeup interval is ',ISLEEP,' seconds'
*
*     Set logging level
*
      CALL GETENVF('FMLOGL',CHLOG)
      IF(IS(1).GT.0) ILOG = ICDECI(CHLOG,1,8)
      CALL FMLOGL(ILOG)
*
*     Find queue directory
*
      CALL GETENVF('FMCHEOPS',CHDIR)
      IF(IS(1).EQ.0) THEN
         CHDIR = '/fatmen/fmcheops'
         LDIR  = 16
         IF(IDEBFA.GE.0) PRINT *,'CHEOPS. queue directory defaulted ',
     +      'to ',CHDIR(1:LDIR)
      ELSE
         LDIR  = IS(1)
         IF(IDEBFA.GE.0) PRINT *,'CHEOPS. queue directory is ',
     +      CHDIR(1:LDIR)
      ENDIF
*
*     Lift banks for source and destination
*
      JBIAS = 2
      CALL MZBOOK(IXSTOR,LSRCBK,LSUP,JBIAS,'DSFA',0,0,NWDSFA,IODSFA,0)
      CALL MZBOOK(IXSTOR,LDSTBK,LSUP,JBIAS,'DSFA',0,0,NWDSFA,IODSFA,0)
 
      ICONT = 0
   10 CONTINUE
*
*     Look for new files in queue directory
*
      INQUIRE (FILE=CHDIR(1:LDIR)//'/signal.stop', EXIST=EXIST)
      IF(EXIST) THEN
         PRINT *,'CHEOPS. signal.stop file found - stopping'
         GOTO 80
      ENDIF
*
*     Zero/blank banks according to I/O characteristic
*
      CALL DZZERO(IXSTOR,LSRCBK)
      CALL DZZERO(IXSTOR,LDSTBK)
 
      CHFILE = ' '
      ISTAT = FAFNDF(CHDIR(1:LDIR),CHNAME,ICONT)
      IF(ISTAT.NE.0) THEN
         IF(IDEBFA.GE.1) THEN
            CALL DATIME(ID,IT)
            PRINT *,'CHEOPS. time is ',ID,IT,
     +      ' sleeping for ',ISLEEP,' seconds'
         ENDIF
         CALL SLEEPF(ISLEEP)
         ICONT = 0
         GOTO 10
      ENDIF
 
      ICONT  = 1
*
*     Stop?
*
      IF(INDEX(CHNAME,'signal.stop').NE.0) THEN
         PRINT *,'CHEOPS. signal.stop file found - stopping'
         GOTO 80
      ENDIF
 
      CHFILE = CHDIR(1:LDIR) // '/' // CHNAME
*
*     Ignore files . and ..
*
      IF((CHFILE(1:1).EQ.'.')) GOTO 10
 
      LFILE = LENOCC(CHFILE)
      IF(IDEBFA.GE.0) THEN
         CALL DATIME(ID,IT)
         PRINT *,'CHEOPS. time is ',ID,IT
         PRINT *,'CHEOPS. new file found --> ',CHFILE(1:LFILE)
      ENDIF
 
      IF(INDEX(CHFILE(1:LFILE),'.rqid').NE.0)    THEN
*
*     Get request ID and store in FATMEN catalogue
*
         OPEN(LUNREQ,FILE=CHFILE(1:LFILE),STATUS='OLD',IOSTAT=IOSTAT,
     +        FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=90)
   20    CONTINUE
*
*     Look for status message
*
         CHCOMM = ' '
         READ(LUNREQ,'(A)',END=40) CHLINE
         LLINE  = LENOCC(CHLINE)
         IF(CHLINE(1:1).EQ.'#') THEN
            IF(IDEBFA.GE.3) PRINT *,'CHEOPS. comment line skipped: ',
     +         CHLINE(1:LLINE)
            GOTO 20
         ENDIF
 
         IF(IDEBFA.GE.3) PRINT *,'CHEOPS. status line: ',
     +      CHLINE(1:LLINE)
 
         IERROR = INDEX(CHLINE(1:LLINE),'STATUS=ERROR')
         IF(IERROR.EQ.0) THEN
            CHCOMM = 'copy successfully queued to CHEOPS. '
            LCOMM  = 26
         ELSE
            CHCOMM = 'CHEOPS error: '
            LCOMM  = 14
         ENDIF
*
*     Get request id / error message
*
         READ(LUNREQ,'(A)',END=40) CHLINE
         LLINE  = LENOCC(CHLINE)
         LLINE  = MIN(80,LCOMM+LLINE)
         CHCOMM(LCOMM:) = CHLINE(1:LLINE)
         IF(IDEBFA.GE.3) PRINT *,'CHEOPS. request # / error message: ',
     +      CHLINE(1:LLINE)
*
*     Now read rest of file and fill source/destination banks
*
   30    CONTINUE
         READ(LUNREQ,'(A)',END=40) CHLINE
         LLINE  = LENOCC(CHLINE)
         IF(IDEBFA.GE.3) PRINT *,'CHEOPS. read: ',
     +      CHLINE(1:LLINE)
 
         IF(CHLINE(1:2).EQ.'S_') THEN
            CALL FMATOB(CHSRCE,LSRCBK,CHLINE,IRC)
         ELSEIF(CHLINE(1:2).EQ.'D_') THEN
            CALL FMATOB(CHDEST,LDSTBK,CHLINE,IRC)
         ENDIF
 
         GOTO 30
 
   40    CONTINUE
*
*     Update comment field
*
      CALL UCTOH(CHCOMM,IQ(LDSTBK+MUCMFA),4,80)
*
*     Now update catalogue
*
         CALL FMBTOF(CHDEST,IRC)
         CLOSE(LUNREQ,STATUS='DELETE')
 
      ELSEIF(INDEX(CHFILE(1:LFILE),'.rep').NE.0) THEN
*
*     Get completion status and update catalogue
*
         OPEN(LUNREQ,FILE=CHFILE(1:LFILE),STATUS='OLD',IOSTAT=IOSTAT,
     +        FORM='FORMATTED',ACCESS='SEQUENTIAL',ERR=90)
         JD    = 0
         JCODE = 0
         JSTR  = 0
   50    CONTINUE
         READ(LUNREQ,'(A)',END=70) CHLINE
         LLINE = LENOCC(CHLINE)
         IF(CHLINE(1:11).EQ.'COMPL_CODE=') THEN
            ICODE = ICDECI(CHLINE,12,LLINE)
            JCODE = 1
            IF(IDEBFA.GE.3) PRINT *,'CHEOPS. completion code: ',
     +         ICODE
         ELSEIF(CHLINE(1:10).EQ.'COMPL_STR=') THEN
            CHSTR = CHLINE(11:LLINE)
            LSTR  = LLINE - 11 + 1
            JSTR  = 1
         ELSEIF(CHLINE(1:5).EQ.'RQID=') THEN
            CHREQ = CHLINE(6:LLINE)
            JD    = 1
            IF(IDEBFA.GE.3) PRINT *,'CHEOPS. RQID: ',CHLINE(6:LLINE)
         ENDIF
 
         IF(JCODE.EQ.0.OR.JD.EQ.0.OR.JSTR.EQ.0) GOTO 50
*
*     Was it successful?
*
         IF(ICODE.NE.0) THEN
            CHCOMM  = CHSTR(1:LSTR)
            LCOMM   = LSTR
            IF(IDEBFA.GE.3) PRINT *,'CHEOPS. error text: ',
     +         CHCOMM(1:LCOMM)
         ENDIF
*
*     Now read rest of file and fill source/destination banks
*
   60    CONTINUE
         READ(LUNREQ,'(A)',END=70) CHLINE
         LLINE  = LENOCC(CHLINE)
 
         IF(IDEBFA.GE.3) PRINT *,'CHEOPS. read: ',
     +      CHLINE(1:LLINE)
 
         IF(CHLINE(1:2).EQ.'S_') THEN
            CALL FMATOB(CHSRCE,LSRCBK,CHLINE,IRC)
         ELSEIF(CHLINE(1:2).EQ.'D_') THEN
            CALL FMATOB(CHDEST,LDSTBK,CHLINE,IRC)
         ENDIF
 
         GOTO 60
 
   70    CONTINUE
*
*     Update comment field if transfer failed
*
         IF(ICODE.NE.0) CALL UCTOH(CHCOMM,IQ(LDSTBK+MUCMFA),4,LCOMM)
*
*     Now update catalogue
*
         CALL FMBTOF(CHDEST,IRC)
         CLOSE(LUNREQ,STATUS='DELETE')
 
      ELSEIF(INDEX(CHFILE(1:LFILE),'.BAD').NE.0) THEN
*
*     Error in request - report to user
*
      ELSEIF(INDEX(CHFILE(1:LFILE),'.req').NE.0) THEN
*
*     Queue file - ignore quietly
*
         IF(IDEBFA.GE.3) PRINT *,'CHEOPS. queue file: ',
     +      CHFILE(1:LFILE),' (ignored quietly)'
      ELSE
         IF(IDEBFA.GE.0) PRINT *,'CHEOPS. unrecognised file: ',
     +      CHFILE(1:LFILE),' (ignored)'
      ENDIF
 
      GOTO 10
 
   80 CONTINUE
 
      STOP
 
   90 PRINT *,'CHEOPS. error ',IOSTAT,' opening ',CHFILE(1:LFILE)
 
      END
