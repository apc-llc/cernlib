*
* $Id: xzputf.F,v 1.5 1998/09/25 09:25:31 mclareni Exp $
*
* $Log: xzputf.F,v $
* Revision 1.5  1998/09/25 09:25:31  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/10/23 13:26:27  mclareni
* NT mods
*
* Revision 1.3  1997/09/02 08:46:27  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.2  1997/01/17 08:56:17  gunter
* call vxinvb for linux too.
*
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPUTF(LOCAL,REMOTE,LRECL,LFORM,RRECL,RFORM,CHOPT,IRC)
*              A - local file has already been opened
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
#if defined(CERNLIB_IBMVM)
      CHARACTER*13  CHTIME
      CHARACTER*1   RECFM
      CHARACTER*255 CHFILE,CHTEMP
#endif
      CHARACTER*(*) LOCAL,REMOTE,LFORM,RFORM
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
#include "cspack/quest.inc"
      CHARACTER*4  CHOPE,CHOPO
      CHARACTER*80 CARD
      CHARACTER*8  DELTIM
      DIMENSION    IUHEAD(400)
      DIMENSION    IOCR(100)
      PARAMETER (JBIAS=2)
      PARAMETER (MEGA=1024*1024)
      COMMON/FZSTAT/INFLUN,INFSTA,INFOFZ(40)
      INTEGER      RRECL
      DIMENSION    ITEST(5)
#if defined(CERNLIB_IPSC)
      DATA         IPATT/'0123CDEF'X/
#elif defined(CERNLIB_WINNT)
      DATA         IPATT/Z'0123CDEF'/
#elif defined(CERNLIB_DECS) || defined(CERNLIB_LINUX) 
      DATA         IPATT/X'0123CDEF'/
#elif defined(CERNLIB_VAX)
      DATA         IPATT/Z0123CDEF/
#endif
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
      LENL = LENOCC(LFORM)
      LENR = LENOCC(RFORM)
 
      IF(IOPTC.NE.0) THEN
         LENR = LENR + 1
         RFORM(LENR:LENR) = 'C'
      ENDIF
 
      IF(LENL.EQ.0) THEN
         LFORM = 'Z'
         LENL  = 1
      ENDIF
 
      IF(LENR.EQ.0) THEN
         RFORM = LFORM
         LENR  = LENL
      ENDIF
 
      IF(IDEBXZ.GE.3)
     +PRINT *,'XZPUTF. enter for ',LOCAL(1:NCHL),' ',REMOTE(1:NCHR),
     +        LRECL,' ',LFORM,RRECL,' ',RFORM,CHOPT
 
      CHOPE = ' '
      IF(INDEX(LFORM(1:LENL),'A').NE.0) THEN
*
*     Exchange file format, ASCII mapping
*
         LRECL = 80
         CHOPE = 'F'
 
      ELSEIF(INDEX(LFORM(1:LENL),'X').NE.0) THEN
         IF(INDEX(LFORM(1:LENL),'D').NE.0) CHOPE = 'D'
      ELSEIF(INDEX(LFORM(1:LENL),'Z').NE.0) THEN
         CHOPE = 'V'
      ENDIF
 
      LOPT = LENOCC(CHOPE) + 1
      IF(IOPTC.NE.0) CHOPE(LOPT:LOPT) = 'C'
 
      IF(IOPTA.EQ.0) THEN
#if defined(CERNLIB_IBMVM)
*
*     Get file information
*
      CHTEMP = LOCAL(1:NCHL)
      CALL CTRANS('.',' ',CHTEMP,1,NCHL)
      CALL VMQFIL(CHTEMP(1:NCHL),RECFM,LBLK,NRECS,
     +   NBLOCKS,CHTIME,ISTAT,IRC)
*
*     Get file mode
*
      INQUIRE(FILE='/'//CHTEMP(1:NCHL),NAME=CHFILE)
      LCHF = LENOCC(CHFILE)
*
*     OS simulation? Assume VBS...
*
      IF(CHFILE(LCHF:LCHF).EQ.'4') THEN
         WRITE(CHFILE,9002) LUNXZI,CHTEMP(1:NCHL),LBLK
9002     FORMAT('FILEDEF ',I2,' DISK ',A,
     +          ' (RECFM VBS LRECL 32756 BLOCK ',I6)
         LCHF = LENOCC(CHFILE)
         CALL VMCMS(CHFILE(1:LCHF),IRC)
         IF(IDEBXZ.GE.3) PRINT *,'XZPUTF. rc from ',
     +      CHFILE(1:LCHF),' = ',IRC
         OPEN(LUNXZI,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=IRC)
      ELSE
         CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,CHOPE,IRC)
      ENDIF
#endif
#if !defined(CERNLIB_IBMVM)
      LCHOPE = LENOCC(CHOPE) + 1
      IF(IOPTC.NE.0) CHOPE(LCHOPE:LCHOPE) = 'C'
      CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,CHOPE,IRC)
#endif
      IF(IRC.NE.0) THEN
         PRINT *,'XZPUTF. cannot open local file'
         GOTO 95
      ENDIF
      ENDIF
 
      IF((INDEX(LFORM(1:LENL),'X').NE.0).AND.(LRECL.EQ.0)) THEN
*
*     Get record length from file
*
         IF(INDEX(CHOPE,'D').EQ.0) THEN
            READ(LUNXZI) ITEST
            REWIND(LUNXZI)
         ELSE
            READ(LUNXZI,REC=1) ITEST
         ENDIF
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
         IF(ITEST(1).NE.IPATT) CALL VXINVB(ITEST(5),1)
#endif
#if !defined(CERNLIB_CRAY)
         LRECL = JBYT(ITEST(5),1,24) * 4
#endif
#if defined(CERNLIB_CRAY)
         LRECL = JBYT(ITEST(3),1,24) * 4
#endif
      ENDIF
 
      IF((INDEX(LFORM(1:LENL),'Z').NE.0).AND.(LRECL.EQ.0)) THEN
         IF(IDEBXZ.GE.-3) PRINT *,'XZPUTF. record length for ',
     +      'native FZ file defaulted to 3600 bytes'
         LRECL = 3600
      ENDIF
 
      IF(RRECL.EQ.0) RRECL = LRECL
      IF(LRECL.EQ.0) THEN
         PRINT *,'XZPUTF. the record length (in bytes) must be given'
         IRC = 1
         RETURN
      ENDIF
 
      CALL FZFILE(LUNXZI,LRECL/4,LFORM(1:LENL))
 
      CALL FZLOGL(LUNXZI,IDEBXZ)
      CALL FZLOGL(LUNFZO,IDEBXZ)
 
      WRITE(CARD,9001) REMOTE(1:NCHR),RRECL,RFORM(1:LENR)
 9001 FORMAT('PUTFZ:',A,' ',I6,A4)
      CALL CZPUTA(CARD,ISTAT)
*
*     Verify that FZ file has been opened by server
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
      IF(CHMAIL(1:2).NE.'OK')GO TO 90
*
*     Perform the transfer
*
      CALL CZRTIM(DELTIM)
      CALL TIMED(T)
      NR = 0
    1 CONTINUE
      NUH = 400
      CALL FZIN(LUNXZI,IHDIV,LSUP,JBIAS,' ',NUH,IUHEAD)
      IF((IQUEST(1).LT.0).OR.(IQUEST(1).GE.4)) GOTO 2
      IF(IQUEST(1).EQ.0) THEN
         IEVENT = IQUEST(11)
      ENDIF
*
*     start of run
*
      IF(IQUEST(1).EQ.1) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZPUTF. read start of run'
         IF(IOPTR.NE.0) GOTO 1
         CALL FZRUN(LUNFZO,IQUEST(11),NUH,IUHEAD)
         GOTO 1
*
*     end of run
*
      ELSEIF(IQUEST(1).EQ.2) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZPUTF. read end of run'
         IF(IOPTR.NE.0) GOTO 1
         CALL FZRUN(LUNFZO,-1,NUH,IUHEAD)
         GOTO 1
*
*     ZEBRA eof
*
      ELSEIF(IQUEST(1).EQ.3) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZPUTF. read end of file'
         IF(IOPTZ.NE.0) GOTO 1
         CALL FZENDO(LUNFZO,'E')
         GOTO 2
      ENDIF
 
      IF(IDEBXZ.GE.3) PRINT *,'XZPUTF. read ',IQUEST(14),
     +   ' data words + ',NUH,' words of user header'
      CALL UCOPY(IQUEST(21),IOCR(1),MIN(IQUEST(20),100))
      CHOPO = 'L'
      IF(IQUEST(14).EQ.0) CHOPO = 'Z'
      CALL FZOUT(LUNFZO,IHDIV,LSUP,IEVENT,CHOPO,IOCR(1),NUH,IUHEAD)
      IF(CHOPO.EQ.'L') CALL MZDROP(IHDIV,LSUP,' ')
      GOTO 1
    2 CONTINUE
      IUHEAD(1) = 999
      IEVENT    = 1
      IOCR(1)   = 2
      CALL FZOUT(LUNFZO,IHDIV,LSUP,IEVENT,'Z',IOCR(1),1,IUHEAD)
      CALL FZENDO(LUNFZO,'F')
      IF(IDEBXZ.GE.0) PRINT 2000
 2000 FORMAT(' File transfer completed')
*
*     Call FZINFO to get NWORDS, NMEGA transferred
*
      CALL FZINFO(LUNXZI)
      CALL FZENDI(LUNXZI,'T')
      IF(INFLUN.NE.LUNXZI) THEN
         PRINT *,'XZPUTF. error obtaining FZINFO for LUN = ',LUNXZI
         MBYTES = 0
      ELSE
         MBYTES = (INFOFZ(19) + INFOFZ(20)/MEGA)*IQCHAW
      ENDIF
 
      CLOSE(LUNXZI)
 
      CALL CZRTIM(DELTIM)
      CALL TIMED(T)
 
      IF(INDEX(CHOPT,'S').NE.0) THEN
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         RATE   = FLOAT(MBYTES)*1000./FLOAT(NSECS)
         NKILO  = MBYTES/1024
#include "cspack/xzstat.inc"
         PRINT *,' Transferred ',NKILO,' KB, rate = ',RATE,' KB/S'
         PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' sec.'
      ENDIF
 
*
   80 CLOSE(LUNXZI)
      GO TO 99
*
   90 PRINT *,' Cannot open remote file'
      IRC = 1
      CALL FZENDI(LUNXZI,'T')
      GO TO 80
*
   95 PRINT *,' Cannot open local file'
      IRC = 2
*
   99 END
