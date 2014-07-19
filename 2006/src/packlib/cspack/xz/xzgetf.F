*
* $Id: xzgetf.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgetf.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETF(LOCAL,REMOTE,LRECL,LFRM,RRECL,RFRM,CHOPT,IRC)
*              A - local file has already been opened
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
      CHARACTER*(*) LOCAL,REMOTE,LFRM,RFRM
      CHARACTER*8  DELTIM
      CHARACTER*4  CHOPE,CHOPO,LFORM,RFORM
#if defined(CERNLIB_IBMVM)
      CHARACTER*255 CHFILE,CHTEMP
      CHARACTER*2   CHMODE
#endif
      CHARACTER*80 CARD
      DIMENSION    IUHEAD(400)
      DIMENSION    IOCR(100)
      PARAMETER (JBIAS=2)
      PARAMETER (MEGA=1024*1024)
      COMMON/FZSTAT/INFLUN,INFSTA,INFOFZ(40)
      INTEGER      RRECL
#include "cspack/quest.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
#if defined(CERNLIB_IBMVM)
      LBLK  = 16384
#endif
      IRC   = 0
      NCHL  = LENOCC(LOCAL)
      NCHR  = LENOCC(REMOTE)
      LFORM = LFRM
      RFORM = RFRM
      LENL = LENOCC(LFORM)
      LENR = LENOCC(RFORM)
 
      IF(LENR.EQ.0) THEN
         RFORM = 'Z'
         LENR  = 1
      ENDIF
 
      IF(LENL.EQ.0) THEN
         LFORM = RFORM
         LENL  = LENR
      ENDIF
 
      IF(IOPTC.NE.0) THEN
         LENR = LENR + 1
         RFORM(LENR:LENR) = 'C'
      ENDIF
 
      IF(IDEBXZ.GE.1)
     +PRINT *,'XZGETF. enter for ',LOCAL(1:NCHL),' ',REMOTE(1:NCHR),
     +        LRECL,' ',LFORM,RRECL,' ',RFORM,CHOPT
 
      CHOPE = 'ON'
 
      IF(INDEX(RFORM(1:LENR),'A').NE.0) THEN
*
*     Exchange file format, ASCII mapping
*
         RRECL = 80
         CHOPE = 'FON'
 
      ELSEIF(INDEX(RFORM(1:LENR),'X').NE.0) THEN
*
*     Binary exchange format
*
         IF(INDEX(RFORM(1:LENR),'D').NE.0)  CHOPE = 'DON'
      ELSEIF(INDEX(RFORM(1:LENR),'Z').NE.0) THEN
         CHOPE = 'VON'
      ELSE
 
         IF(RRECL.EQ.0) THEN
            IF(IDEBXZ.GE.-3) PRINT *,'XZPUTF. record length for ',
     +         'native FZ file defaulted to 3600 bytes'
            RRECL = 3600
*           PRINT *,'XZGETF. the record length (in bytes) must be '
*    +      //'given for native mode files'
*           IRC = 1
*           RETURN
         ENDIF
 
      ENDIF
 
      WRITE(CARD,9001) REMOTE(1:NCHR),RRECL,RFORM(1:LENR)
 9001 FORMAT('GETFZ:',A,' ',I6,A4)
      CALL CZPUTA(CARD,ISTAT)
*
*     Get remote record length
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
      READ(CHMAIL,'(I6)') JRECL
      IF(JRECL.LT.0)GO TO 90
*
*     Take record length from remote file
*
      IF(LRECL.EQ.0) LRECL = JRECL
 
      IF(IOPTA.EQ.0) THEN
#if defined(CERNLIB_IBMVM)
      IF(LFORM(1:LENL).EQ.'Z') THEN
         CHTEMP = LOCAL(1:NCHL)
         CALL CTRANS('.',' ',CHTEMP,1,NCHL)
         LASTB  = INDEXB(CHTEMP(1:NCHL),' ')
         IF(INDEX(CHTEMP(1:NCHL),' ').EQ.LASTB) THEN
            CHTEMP(NCHL+1:NCHL+3) = ' A4'
            NCHT = NCHL + 3
         ELSE
            CHTEMP(LASTB+2:LASTB+2) = '4'
            NCHT = LASTB + 2
         ENDIF
         WRITE(CHFILE,9002) LUNXZO,CHTEMP(1:NCHT),LBLK
9002     FORMAT('FILEDEF ',I2,' DISK ',A,
     +          ' (RECFM VBS LRECL 32756 BLOCK ',I6,' PERM)')
         LCHF = LENOCC(CHFILE)
         CALL VMCMS(CHFILE(1:LCHF),IRC)
         IF(IDEBXZ.GE.3) PRINT *,'XZGETF. rc from ',
     +      CHFILE(1:LCHF),' = ',IRC
         OPEN(LUNXZO,STATUS='NEW',FORM='UNFORMATTED',IOSTAT=IRC)
      ELSE
         CALL SZOPEN(LUNXZO,LOCAL(1:NCHL),LRECL,CHOPE,IRC)
      ENDIF
#endif
#if !defined(CERNLIB_IBMVM)
      LCHOPE = LENOCC(CHOPE) + 1
      IF(IOPTC.NE.0) CHOPE(LCHOPE:LCHOPE) = 'C'
      CALL SZOPEN(LUNXZO,LOCAL(1:NCHL),LRECL,CHOPE,IRC)
#endif
      IF(IRC.NE.0) THEN
         PRINT *,'XZGETF. cannot open local file'
         GOTO 95
      ENDIF
      ENDIF
 
      IF(LFORM(1:LENL).EQ.'Z') LFORM = ' '
      CALL FZFILE(LUNXZO,LRECL/4,'FO'//LFORM(1:LENL))
 
      CALL FZLOGL(LUNXZO,IDEBXZ)
      CALL FZLOGL(LUNFZI,IDEBXZ)
*
*     Perform the transfer
*
      CALL CZRTIM(DELTIM)
      CALL TIMED(T)
      NR  = 0
    1 CONTINUE
      NUH = 400
      CALL FZIN(LUNFZI,IHDIV,LSUP,JBIAS,' ',NUH,IUHEAD)
      IF((IQUEST(1).LT.0).OR.(IQUEST(1).GE.4)) GOTO 2
      NR  = NR + 1
      IF(IQUEST(1).EQ.0) THEN
         IEVENT = IQUEST(11)
         IF((NUH.EQ.1).AND.(IUHEAD(1).EQ.999)
     +                .AND.(IQUEST(14).EQ.0)) GOTO 2
      ENDIF
*
*     start of run
*
      IF(IQUEST(1).EQ.1) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZGETF. read start of run'
         IF(IOPTR.NE.0) GOTO 1
         CALL FZRUN(LUNXZO,IQUEST(11),NUH,IUHEAD)
         GOTO 1
*
*     end of run
*
      ELSEIF(IQUEST(1).EQ.2) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZGETF. read end of run'
         IF(IOPTR.NE.0) GOTO 1
         CALL FZRUN(LUNXZO,-1,NUH,IUHEAD)
         GOTO 1
*
*     ZEBRA eof
*
      ELSEIF(IQUEST(1).EQ.3) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZGETF. read end of file'
         IF(IOPTZ.NE.0) GOTO 1
         CALL FZENDO(LUNXZO,'E')
         GOTO 2
      ENDIF
 
      IF(IDEBXZ.GE.3) PRINT *,'XZGETF. read ',IQUEST(14),
     +   ' data words + ',NUH,' words of user header'
      CALL UCOPY(IQUEST(21),IOCR,MIN(IQUEST(20),100))
      CHOPO = 'L'
      IF(IQUEST(14).EQ.0) CHOPO = 'Z'
      CALL FZOUT(LUNXZO,IHDIV,LSUP,IEVENT,CHOPO,IOCR,NUH,IUHEAD)
      IF(CHOPO.EQ.'L') CALL MZDROP(IHDIV,LSUP,' ')
      GOTO 1
    2 CONTINUE
      IF(IDEBXZ.GE.0) PRINT 2000
 2000 FORMAT(' File transfer completed')
*
*     Call FZINFO to get NWORDS, NMEGA transferred
*
      CALL FZINFO(LUNXZO)
      CALL FZENDO(LUNXZO,'T')
      IF(INFLUN.NE.LUNXZO) THEN
         PRINT *,'XZGETF. error obtaining FZINFO for LUN = ',LUNXZO
         MBYTES = 0
      ELSE
         MBYTES = (INFOFZ(19) + INFOFZ(20)/MEGA)*IQCHAW
      ENDIF
      CLOSE(LUNXZO)
 
      CALL CZRTIM(DELTIM)
      CALL TIMED(T)
 
      IF(INDEX(CHOPT,'S').NE.0) THEN
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         NKILO = MBYTES/1024
         RATE  = FLOAT(MBYTES)*1000./FLOAT(NSECS)
#include "cspack/xzstat.inc"
         PRINT *,' Transferred ',NKILO,' KB, rate = ',RATE,' KB/S'
         PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' sec.'
      ENDIF
 
*
   80 CLOSE(LUNXZO)
      GO TO 99
*
   95 PRINT *,' Cannot open local file'
      IRC = 2
      CALL FZENDI(LUNXZO,' ')
      GO TO 80
*
   90 PRINT *,' Cannot open remote file'
      IRC = 1
*
   99 END
