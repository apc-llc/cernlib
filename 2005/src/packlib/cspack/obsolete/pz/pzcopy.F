*
* $Id: pzcopy.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: pzcopy.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
      SUBROUTINE PZCOPY
 
      CHARACTER*255 CHFILE,CHLINK,CHREM
      CHARACTER*20  CHUSR,CHGRP,CHPASS
      CHARACTER*4   CHOPT
      CHARACTER*8   DELTIM
      DIMENSION     IBUFF(8192)
#include "hcmail.inc"
#include "czsock.inc"
#include "quest.inc"
#include "czunit.inc"
#if defined(CERNLIB_VAXVMS)
#include "vmsinf.inc"
      INCLUDE       '($RMSDEF)'
      EXTERNAL      XZFINF
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*4   RECFM
      CHARACTER*13  CHTIME
#endif
#if defined(CERNLIB_UNIX)
      CHARACTER*80  CHBUF,CHHOME
      INTEGER       STATF
      DIMENSION     INFO(12)
#endif
#include "pzunit.inc"
#include "slate.inc"
      PARAMETER     (MEGA=1024*1024)
      DATA NENTRY/0/
      SAVE NENTRY,CHUSR,CHGRP,LUSR,LGRP
 
      CALL KUGETS(CHFILE,LFILE)
      CALL KUGETS(CHLINK,LLINK)
      CALL KUGETC(CHOPT ,LCHOPT)
      IF(LCHOPT.EQ.0) CHOPT = ' '
      IOPTS = INDEX(CHOPT,'S')
 
      IF(LFILE.EQ.0) RETURN
*
*     Connect to PIAF on first entry
*
      IF(NENTRY.EQ.0) THEN
*
*     Get username and password from netrc file
*
         CALL PZNTRC(LUNPZN,CHGRP,CHPASS,'C',IRC)
         IF(IRC.NE.0) RETURN
         LGRP  = LENOCC(CHGRP)
*
*     Get current username
*
         CHUSR = ' '
         CALL PZUSER(CHUSR)
         LUSR = LENOCC(CHUSR)
         CALL CUTOL(CHUSR(1:LUSR))
*
*     Connect
*
         CALL CZOPEN('ZSERV','piaf1',IRC)
 
         IF(IRC.NE.0) THEN
            PRINT *,'PZCOPY. cannot open connection to PIAF'
            CALL PZNTRC(LUNPZN,' ',' ','R',IRC)
            RETURN
         ENDIF
 
         NENTRY = 1
 
      ENDIF
 
#if defined(CERNLIB_IBMVM)
      JFILE = 1
#endif
#if defined(CERNLIB_VAXVMS)
      JFILE = INDEX(CHFILE(1:LFILE),']')+1
#endif
#if defined(CERNLIB_UNIX)
      JFILE = INDEXB(CHFILE(1:LFILE),'/')+1
#endif
*
*     Build remote file name
*
      IF(LLINK.EQ.0) THEN
         CHREM = '~'//CHGRP(1:LGRP)//'/'//CHUSR(1:LUSR)
     +           //'/'//CHFILE(JFILE:LFILE)
      ELSE
         IF(INDEX(CHLINK(1:LLINK),'/').EQ.0) THEN
            CHREM = '~'//CHGRP(1:LGRP)//'/'//CHUSR(1:LUSR)
     +              //'/'//CHLINK(1:LLINK)
         ELSE
            CHREM = '~'//CHGRP(1:LGRP)//'/'//CHLINK(1:LLINK)
         ENDIF
      ENDIF
      LREM  = LENOCC(CHREM)
*
*     Now send the file
*
      LRECL = 0
      CALL RZOPEN(LUNXZI,'RZ',CHFILE(1:LFILE),'P',LRECL,ISTAT)
      IF(ISTAT.NE.0) THEN
         PRINT *,'Cannot open local file'
         RETURN
      ENDIF
*
*     Get number of records in the file
*
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',CHFILE,1,LFILE)
      CALL CLTOU(CHFILE(1:LFILE))
      CALL VMQFIL(CHFILE(1:LFILE),RECFM,LRECL,NRECS,NBLKS,
     +            CHTIME,ISTAT,IRC)
#endif
#if defined(CERNLIB_VAXVMS)
      CALL LIB$GET_LUN(LUNDEC)
      OPEN(LUNDEC,FILE=CHFILE(1:LFILE),STATUS='OLD',READONLY,
     +     IOSTAT=IRC,USEROPEN=XZFINF)
      CALL LIB$FREE_LUN(LUNDEC)
      NRECS = (IBLK*128)/LRECL
#endif
#if defined(CERNLIB_UNIX)
      ISTAT = STATF(CHFILE(1:LFILE),INFO)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SGI))
      NRECS = (INFO(12)*128)/LRECL
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SGI))
*
*     No longer necessary with IRIX 5
*
      NRECS = (INFO(7)/4)/LRECL
#endif
 
      SIZE = FLOAT(NRECS*LRECL*4)/MEGA
      PRINT 8001,NRECS,LRECL,SIZE
8001  FORMAT(' Transferring ',I6,' records of length ',I6,' words = ',
     +       F10.2,' MB')
 
      ISIZE = (SIZE+.5)
 
      PRINT 8002,CHFILE(1:LFILE),CHREM(1:LREM)
8002  FORMAT(' Local file: ',A,' remote file: ',A)
 
      LRECL = LRECL * 4
*
*     Issue remote SFGET
*
      WRITE(CHMAIL,8003) ISIZE,CHREM(1:LREM),CHOPT
8003  FORMAT('PPIAF:',I6,A,1X,A)
      CALL CZPUTA(CHMAIL,ISTAT)
 
      CALL CZGETA(CHMAIL,ISTAT)
 
      LLINK = LENOCC(CHMAIL) - 1
      CHLINK = CHMAIL(2:LLINK+1)
      ISLASH = INDEX(CHREM(1:LREM),'/')
 
#include "czmess.inc"
      IF(IRC.NE.0) GOTO 90
*
*     Create remote file
*
      CHNODE(LUNXZO) = 'piaf1'
      JSKIN(LUNXZO)  = ISKIN
      JSKOUT(LUNXZO) = ISKOUT
 
      CALL XZOPEN(LUNXZO,CHLINK(1:LLINK)//CHREM(ISLASH:LREM),
     +            'piaf1',LRECL,'ODR',IRC)
      IF(IRC.NE.0) GOTO 90
*
*     Start timer
*
      IF(IOPTS.NE.0) THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
      ENDIF
*
*     Start transfer
*
      NSTAL = 1
      NSTAR = 1
      CALL XZWRTM(LUNXZI,LUNXZO,IBUFF,NRECS,NSTAL,NSTAR,LRECL,'D',IRC)
      CALL XZCLOS(LUNXZO,' ',IRC)
 
      IF(IOPTS.NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         NKILO = SIZE * 1024
         NR    = NREC
         RATE  = (SIZE*1024.)/FLOAT(NSECS)
#include "xzstat.inc"
         PRINT 8004,NRECS,RATE
8004  FORMAT(' Transferred ',I6,' records, transfer rate = ',F10.2,
     +           ' KB/S')
         PRINT 8005,DELTIM,T
8005  FORMAT(' Elapsed time = ',A,' CP time = ',F5.2,' sec.')
      ENDIF
 
 90   CONTINUE
      CLOSE(LUNXZI)
 
      END
