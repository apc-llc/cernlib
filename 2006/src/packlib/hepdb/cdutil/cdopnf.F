*
* $Id: cdopnf.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdopnf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOPNF(LUN,FILEN,LRECL,CHOPT,IRC)
      CHARACTER*(*) FILEN
      CHARACTER*80  FNAME,CHNAME
      CHARACTER*80  MVSDSN
      CHARACTER*12  CHFORM
      CHARACTER*8   CHSTAT,CHREC,CHTYPE
      CHARACTER*80  FILEDEF
#include "hepdb/cduscm.inc"
*
*     JBYTES: conversion factor for open statement
*        RECL = lrecl / jbytes
*
*     On most machines, the record length is specified in bytes
*     except VAX: bytes for unformatted files, 4-byte words otherwise
*            SGI: 4-byte words
*           DECS: 4-byte words
*     Windows/NT: 4-byte words (with DEC Fortran only, 1-byte for F2C version)
*
*
#if defined(CERNLIB_SGI)||defined(CERNLIB_DECS)
      PARAMETER (JBYTES=4)
#endif
#if (defined(CERNLIB_WINNT))&&(!defined(CERNLIB_F2C))
      PARAMETER (JBYTES=4)
#endif
#if (!defined(CERNLIB_SGI))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_WINNT))
      PARAMETER (JBYTES=1)
#endif
#if defined(CERNLIB_IBMMVS)
      CHARACTER*3   CHRECFM
      CHARACTER*10  CHACTION
      LOGICAL       OPN,EXS
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*20  CHGIME
      CHARACTER*80  CHLINE
      CHARACTER*1   MODE
#endif
      LOGICAL       IEXIST
#include "hepdb/quest.inc"
*
*     Open file on unit LUN.
*     CHOPT: C - respect case
*            D - D/A file, LRECL must be specified if N is also given
*            I - open for input
*            O - open for output
*-->         R - RELATIVE organisation (VAX only) <-- Not yet implemented!
*            P - PAM file format
*            F - formatted file (default=unformatted)
*            N - new file
*            V - variable length records (do not specify RECFM=variable on VAX)
*
*     Return codes: 28 - file already exists & New specified
*                    1 - open failed - IOSTAT in IQUEST(1)
*
#include "hepdb/hdbopts.inc"
      IF((IOPTI.EQ.0).AND.(IOPTO.EQ.0)) IOPTI=1
      IRC    = 0
      LF     = LENOCC(FILEN)
      FNAME  = FILEN(1:LF)
#if defined(CERNLIB_IBMVM)
*
*     Crack <user.addr>fn.ft
*
      IF((INDEX(FNAME(1:LF),'<').NE.0).OR.
     +   (INDEX(FNAME(1:LF),'[').NE.0)) THEN
         CALL CTRANS('[','<',FNAME,1,LF)
         CALL CTRANS(']','>',FNAME,1,LF)
         ISTART = INDEX(FNAME(1:LF),'<') + 1
         IEND   = INDEX(FNAME(1:LF),'>') - 1
         ICOL   = INDEX(FNAME(1:LF),':')
*
*     SFS?
*
         IF(ICOL.EQ.0) THEN
            CHGIME = FNAME(ISTART:IEND)
            IDOT   = INDEX(CHGIME,'.')
            IF(IDOT.NE.0) CHGIME(IDOT:IDOT) = ' '
            LCHG   = IEND - ISTART + 1
         ELSE
            CHGIME = FNAME(1:ICOL) // FNAME(ISTART:IEND)
            LCHG   = IEND - ISTART + 1 + ICOL
         ENDIF
         CALL VMCMS(CHGIME(1:LCHG)//' (QUIET NONOTICE STACK)',IRC)
 
         IF(IRC.GT.4) RETURN
 
         CALL VMRTRM(CHLINE,LENGTH)
         MODE  = CHLINE(1:1)
         FNAME = FILEN(IEND+2:LF) // ' ' // MODE
         LF    = LF + 2
      ENDIF
 
      IF(INDEX(FNAME(1:LF),'/').EQ.0) THEN
         FNAME  = '/'//FILEN(1:LF)
         LF     = LF + 1
      ENDIF
 
      DO 1 I=1,LF
    1 IF(FNAME(I:I).EQ.'.') FNAME(I:I) = ' '
*
*     If file exists, find full name and hence file mode
*
      INQUIRE(FILE=FNAME(1:LF),EXIST=IEXIST,NAME=CHNAME)
      LMODE = LENOCC(CHNAME)
      IF(IEXIST) MODE = CHNAME(LMODE:LMODE)
 
#endif
#if defined(CERNLIB_UNIX)
      IF(IOPTC.EQ.0) CALL CUTOL(FNAME)
#endif
#if defined(CERNLIB_IBMMVS)
      CHACTION = 'READWRITE'
      IF(IOPTI.NE.0) CHACTION = 'READ'
      IF(IOPTO.NE.0) CHACTION = 'WRITE'
      CALL XZMVSD(FILEN(1:LF),MVSDSN,LF,ICUT,IRC)
      FNAME  = MVSDSN
      FNAME(1:1) = '/'
#endif
      CHSTAT = 'UNKNOWN'
      IF(IOPTN.NE.0) THEN
         CHSTAT = 'NEW'
         IOPTO  = 1
#if !defined(CERNLIB_VAXVMS)
         INQUIRE(FILE=FNAME(1:LF),EXIST=IEXIST)
         IF(IEXIST) THEN
            IRC = 28
            RETURN
         ENDIF
#endif
      ENDIF
      IF(IOPTI.NE.0) CHSTAT = 'OLD'
#if defined(CERNLIB_APOLLO)
      IF(IOPTO.EQ.0) CHSTAT = 'READONLY'
#endif
      CHFORM = 'UNFORMATTED'
*
*     LRECL on VAX is in words for unformatted files, but bytes otherwise
*
      NBYTES = 4
      IF(IOPTF.NE.0) THEN
         CHFORM = 'FORMATTED'
         NBYTES = 1
      ENDIF
 
      CHREC = 'FIXED'
      IF(IOPTP.NE.0) IOPTV  = 1
      IF(IOPTV.NE.0) CHREC  = 'VARIABLE'
      LCHREC = LENOCC(CHREC)
      IF(IOPTD.EQ.0) THEN
#if defined(CERNLIB_IBMMVS)
      ITRK     = 10
 
      ISECOND  = 10
 
      IDIR     =  0
      IF ( INDEX(FNAME,'(') .NE. 0 ) IDIR = 28
 
      CHRECFM  = 'FB'
      IF(IOPTP.NE.0)  CHRECFM  = 'VBS'
      IF(IOPTV.NE.0)  CHRECFM  = 'VB'
 
      IF(IOPTP.NE.0)  LRECL   =  -1
 
      IBLKSIZE =  4000
      IF(IOPTP.NE.0)  IBLKSIZE =  6232
*------------------------------ check file existence
      INQUIRE ( FILE='/'//FNAME(1:LF), IOSTAT=ISTAT
     +,      EXIST=EXS, OPENED=OPN, NUMBER=LUNOLD )
      IF (IRC .NE. 0)  GO TO 110
 
      IF((.NOT. EXS ).AND.(IOPTI.NE.0)) GOTO 150
*------------------------- ---- input dataset exists not
 
      IF ( .NOT. EXS )  THEN
*------------------------------ define dataset parameters
         CALL FILEINF ( ISTAT,'DEVICE' , '33XX' , 'TRK' , ITRK   ,
     +                   'SECOND' , ISECOND  , 'DIR'   , IDIR   ,
     +                   'RECFM'  , CHRECFM   , 'LRECL' , LRECL ,
     +                   'BLKSIZE', IBLKSIZE )
         IF (ISTAT .NE. 0) GO TO  120
 
      ELSE
*------------------------------ dataset is an old one
         IF (OPN) THEN
*------------------------------ dataset is open
            CLOSE(UNIT=LUNOLD,STATUS='KEEP',IOSTAT=ISTAT)
            IF (ISTAT .NE. 0) GO TO 140
         ENDIF
 
      ENDIF
 
      IF (IOPTD .EQ. 0) THEN
*------------------------------ open dataset
         OPEN(LUN,FILE='/'//FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +        ACTION=CHACTION,IOSTAT=ISTAT)
         IF (ISTAT .NE. 0) GO TO 130
 
      ELSE
*------------------------------ open dataset, accsess = direct
        OPEN(LUN,FILE='/'//FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +       RECL=LRECL,ACCESS='DIRECT',
     +       ACTION=CHACTION,IOSTAT=ISTAT)
        IF (ISTAT .NE. 0) GO TO 130
 
      ENDIF
 
      RETURN
 
*------------------------------ error handling
  110 WRITE(6,*) 'CDOPNF - INQUIRE ERROR - IOSTAT = ',ISTAT
      GO TO 99
 
  120 WRITE(6,*) 'CDOPNF - FILEINF ERROR - ISTAT = ',ISTAT
      GO TO 99
 
  130 WRITE(6,*) 'CDOPNF - OPEN ERROR - IOSTAT = ',ISTAT
      GO TO 99
 
  140 WRITE(6,*) 'CDOPNF - CLOSE ERROR - IOSTAT = ',ISTAT
      GO TO 99
 
  150 WRITE(6,*) 'CDOPNF - INPUT DATASET NOT ON DISK '
      GO TO 99
#endif
#if defined(CERNLIB_IBMVM)
         IF(IOPTP.NE.0) THEN
*
*     PAM file - issue FILEDEF then open by UNIT
*     (Open by name uses different style DDNAME)
*
            WRITE(FILEDEF,8001) LUN,FNAME(1:LF)
 8001    FORMAT('FILEDEF ',I3,' DISK ',A,
     +          ' (RECFM VBS LRECL 32756 BLOCK 800 PERM)')
            LENF = LENOCC(FILEDEF)
            CALL CTRANS('.',' ',FILEDEF,1,LENF)
            CALL CTRANS('/',' ',FILEDEF,1,LENF)
            CALL VMCMS(FILEDEF(1:LENF),IRC)
            CALL VMCMS('Q FILEDEF',IRC)
            OPEN(LUN,FORM=CHFORM,STATUS=CHSTAT, ERR=99,IOSTAT=ISTAT)
            RETURN
         ELSE
            LENF = LENOCC(FILEDEF)
            CALL CTRANS('.',' ',FILEDEF,1,LENF)
*
*     Only call FILEINF for new files...
*
            IF(IOPTN.NE.0) THEN
               CALL FILEINF(IRC,'RECFM','F','LRECL',LRECL)
            ELSE
               IF(MODE.EQ.'4') CALL FILEINF(IRC,'RECFM','F')
            ENDIF
         ENDIF
#endif
#if !defined(CERNLIB_VAXVMS)
         OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +        ERR=99,IOSTAT=ISTAT)
#endif
#if defined(CERNLIB_VAXVMS)
         INQUIRE(FILE=FNAME(1:LF),EXIST=IEXIST,RECORDTYPE=CHTYPE)
         IF(IEXIST.AND.CHTYPE(1:8).EQ.'VARIABLE') IOPTV = 1
         IF(IOPTO.EQ.0) THEN
*
*     Add READONLY so that files in other people's directories
*     can be accessed...
*
            IF((IOPTN.EQ.0).AND.(IOPTV.EQ.0)) THEN
               OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +         SHARED,BUFFERCOUNT=127,RECORDTYPE=CHREC(1:LCHREC),
     +         RECL=LRECL/NBYTES,IOSTAT=ISTAT,ERR=99,READONLY)
            ELSEIF((IOPTN.NE.0).AND.(IOPTV.EQ.0)) THEN
               OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +         BUFFERCOUNT=127,RECORDTYPE=CHREC(1:LCHREC), RECL=
     +         LRECL/NBYTES,IOSTAT=ISTAT,ERR=99,READONLY)
            ELSE
               OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +         BUFFERCOUNT=127,IOSTAT=ISTAT,ERR=99,READONLY)
            ENDIF
         ELSE
            IF((IOPTN.EQ.0).AND.(IOPTV.EQ.0)) THEN
               OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +         SHARED,BUFFERCOUNT=127,RECORDTYPE=CHREC(1:LCHREC), RECL=
     +         LRECL/NBYTES,IOSTAT=ISTAT, ERR=99)
            ELSEIF((IOPTN.NE.0).AND.(IOPTV.EQ.0)) THEN
               OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +         BUFFERCOUNT=127,RECORDTYPE=CHREC(1:LCHREC), RECL=
     +         LRECL/NBYTES,IOSTAT=ISTAT, ERR=99)
            ELSE
               OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,
     +         BUFFERCOUNT=127,IOSTAT=ISTAT, ERR=99)
*    +         SHARED,BUFFERCOUNT=127, IOSTAT=ISTAT, ERR=99)
            ENDIF
         ENDIF
#endif
*
*     Direct-access files...
*
      ELSE
#if defined(CERNLIB_IBMVM)
         CALL FILEINF(ISTAT,'MAXREC',2)
         IF(ISTAT.NE.0)GO TO 99
         OPEN(UNIT=LUN,FILE=FNAME(1:LF),FORM='UNFORMATTED', RECL=LRECL,
     +   ACCESS='DIRECT',STATUS=CHSTAT,IOSTAT=ISTAT)
         IF(ISTAT.NE.0)GO TO 99
         CLOSE(LUN)
         CALL FILEINF(ISTAT,'MAXREC',16777215)
         IF(ISTAT.NE.0)GO TO 99
#endif
#if !defined(CERNLIB_VAXVMS)
         OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,ERR=99,
     +        RECL=LRECL/JBYTES,
#endif
#if defined(CERNLIB_VAXVMS)
         OPEN(LUN,FILE=FNAME(1:LF),FORM=CHFORM,STATUS=CHSTAT,ERR=99,
     +        SHARED,
     +        RECL=LRECL/NBYTES,
#endif
     +        ACCESS='DIRECT',IOSTAT=ISTAT)
      ENDIF
      RETURN
   99 IRC       = 1
      IQUEST(1) = ISTAT
      END
