*
* $Id: xzfzcp.F,v 1.5 1998/09/25 09:25:00 mclareni Exp $
*
* $Log: xzfzcp.F,v $
* Revision 1.5  1998/09/25 09:25:00  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/10/23 13:26:25  mclareni
* NT mods
*
* Revision 1.3  1997/09/02 08:46:26  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.2  1997/01/17 08:56:16  gunter
* call vxinvb for linux too.
*
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZFZCP(CHFZIN,CHFZOU,IRECL,IFORM,ORECL,OFORM,CHOPT,IRC)
 
      CHARACTER*1   CHFZ
      CHARACTER*(*) CHFZIN,CHFZOU,IFORM,OFORM
      CHARACTER*4   CHOPE,CHOPF
      INTEGER       IRECL,ORECL
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      DIMENSION    IUHEAD(400)
      DIMENSION    IOCR(100)
      PARAMETER (JBIAS=2)
      PARAMETER (MEGA=1024*1024)
      COMMON/FZSTAT/INFLUN,INFSTA,INFOFZ(40)
      INTEGER      RRECL
      DIMENSION    ITEST(5)
#if defined(CERNLIB_IBMVM)
      CHARACTER*13  CHTIME
      CHARACTER*1   RECFM
      CHARACTER*255 CHFILE,CHTEMP
      CHARACTER*2   CHMODE
#endif
#if defined(CERNLIB_IPSC)
      DATA         IPATT/'0123CDEF'X/
#elif defined(CERNLIB_WINNT)
      DATA         IPATT/Z'0123CDEF'/
#elif defined(CERNLIB_DECS) || defined(CERNLIB_LINUX) || defined(CERNLIB_WINNT)
      DATA         IPATT/X'0123CDEF'/
#elif defined(CERNLIB_VAX)
      DATA         IPATT/Z0123CDEF/
#endif
#include "cspack/quest.inc"
#include "cspack/czopts.inc"
 
      IRC    = 0
      LCHFZI = LENOCC(CHFZIN)
      LCHFZO = LENOCC(CHFZOU)
      LCHOPT = LENOCC(CHOPT)
 
      IF(IDEBXZ.GE.1) PRINT *,'XZFZCP. enter for ',
     +   CHFZIN(1:LCHFZI),' ',CHFZOU(1:LCHFZO),' ',
     +   IFORM,' ',OFORM,' ',CHOPT
 
      IF(LCHFZI.EQ.0.OR.LCHFZO.EQ.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZFZCP. error - input or ',
     +      'output file name missing'
         IRC = -1
         GOTO 99
      ENDIF
*
*     Open input file
*
      CHOPE = ' '
      CHOPF = 'XI'
      JRECL = 80
 
#if defined(CERNLIB_UNIX)
      CHOPE = 'D'
      CHOPF = 'DI'
#endif
 
      IF(INDEX(IFORM,'A').NE.0) THEN
*
*     Alpha format?
*
         CHOPE = 'F'
         CHOPF = 'AI'
#if !defined(CERNLIB_UNIX)
      ELSEIF(INDEX(IFORM,'N').NE.0) THEN
         CHOPF = 'XNI'
      ELSEIF(INDEX(IFORM,'Z').NE.0) THEN
*
*     Native file and data format?
*
         CHOPF = 'I'
         IF(IRECL.LE.0) THEN
            IF(IDEBXZ.GE.-3) PRINT *,'XZFZCP. input record length ',
     +         'must be specified for native files'
            IRC = -1
            RETURN
         ENDIF
         JRECL = IRECL
#endif
      ENDIF
 
      LCHOPE   = LENOCC(CHOPE) + 1
      IF(IOPTC.NE.0) CHOPF(LCHOPE:LCHOPE) = 'C'
 
#if defined(CERNLIB_IBMVM)
*
*     Get input file information
*
      CHTEMP = CHFZIN(1:LCHFZI)
      CALL CTRANS('.',' ',CHTEMP,1,LCHFZI)
      CALL VMQFIL(CHTEMP(1:NCHL),RECFM,LBLK,NRECS,
     +   NBLOCKS,CHTIME,ISTAT,IRC)
*
*     Get file mode
*
      INQUIRE(FILE='/'//CHTEMP(1:LCHFZI),NAME=CHFILE)
      LCHF   = LENOCC(CHFILE)
      CHMODE = CHFILE(LCHF-1:LCHF)
*
*     OS simulation? Assume VBS...
*
      IF(CHFILE(LCHF:LCHF).EQ.'4') THEN
         WRITE(CHFILE,9002) LUNXZI,CHTEMP(1:LCHFZI),LBLK
9002     FORMAT('FILEDEF ',I2,' DISK ',A,
     +          ' (RECFM VBS LRECL 32756 BLOCK ',I6)
         LCHF = LENOCC(CHFILE)
         CALL VMCMS(CHFILE(1:LCHF),IRC)
         IF(IDEBXZ.GE.3) PRINT *,'XZFZCP. rc from ',
     +      CHFILE(1:LCHF),' = ',IRC
         OPEN(LUNXZI,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=IRC)
      ELSE
         CALL SZOPEN(LUNXZI,CHFZIN(1:LCHFZI),JRECL,CHOPE,IRC)
      ENDIF
 
#endif
#if !defined(CERNLIB_IBMVM)
      IF(IDEBXZ.GE.3) PRINT *,'XZFZCP. call SZOPEN for ',
     +   CHFZIN(1:LCHFZI),JRECL,CHOPE
      CALL SZOPEN(LUNXZI,CHFZIN(1:LCHFZI),JRECL,CHOPE,IRC)
#endif
*
*     For binary exchange file formats, get record length from file
*
      IF(INDEX(IFORM,'A').EQ.0.AND.INDEX(IFORM,'Z').EQ.0) THEN
 
#if !defined(CERNLIB_UNIX)
         READ(LUNXZI,IOSTAT=ISTAT) ITEST
         REWIND(LUNXZI)
#endif
#if defined(CERNLIB_UNIX)
         READ(LUNXZI,REC=1,IOSTAT=ISTAT) ITEST
#endif
 
         IF(ISTAT.NE.0) THEN
            PRINT *,'XZFZCP. error ',ISTAT,' reading input file'
            CLOSE(LUNXZI)
            GOTO 99
         ENDIF
 
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
         IF(ITEST(1).NE.IPATT) CALL VXINVB(ITEST(5),1)
#endif
#if !defined(CERNLIB_CRAY)
         JRECL = JBYT(ITEST(5),1,24) * 4
#endif
#if defined(CERNLIB_CRAY)
         JRECL = JBYT(ITEST(3),1,24) * 4
#endif
*
*     Check if JRECL is reasonable
*
      IF(JRECL.GT.32756.OR.JRECL.LT.0) THEN
         PRINT *,'XZFZCP. cannot determine record length of ',
     +      'input file. How was this file created?'
         IRC = JRECL
         CLOSE(LUNXZI)
         GOTO 99
      ENDIF
 
      IF(IDEBXZ.GE.2) PRINT *,'XZFZCP. record length of input ',
     +   'file is ',JRECL,' bytes'
*
*     Close and reopen file
*
         CALL SZCLOS(LUNXZI,' ',IRC)
         IF(IDEBXZ.GE.3) PRINT *,'XZFZCP. call SZOPEN for ',
     +      CHFZIN(1:LCHFZI),JRECL,CHOPE
         CALL SZOPEN(LUNXZI,CHFZIN(1:LCHFZI),JRECL,CHOPE,IRC)
 
      ENDIF
 
      IF(IDEBXZ.GE.2) PRINT *,'XZFZCP. call FZFILE for ',
     +   'JRECL = ',JRECL,' chopt = ',CHOPF
      CALL FZFILE(LUNXZI,JRECL/4,CHOPF)
      CALL FZLOGL(LUNXZI,IDEBXZ)
 
      LRECL = JRECL
*
*     Now open the output file
*
      CHOPE = 'ON'
      CHOPF = 'XO'
      JRECL = 80
 
#if defined(CERNLIB_UNIX)
      CHOPE = 'DON'
      CHOPF = 'DO'
#endif
 
      IF(INDEX(OFORM,'A').NE.0) THEN
*
*     Alpha format?
*
         CHOPE = 'FON'
         CHOPF = 'AO'
#if !defined(CERNLIB_UNIX)
      ELSEIF(INDEX(OFORM,'N').NE.0) THEN
         CHOPF = 'XNO'
      ELSEIF(INDEX(OFORM,'Z').NE.0) THEN
*
*     Native file and data format?
*
         CHOPF = 'O'
         IF(ORECL.LE.0) THEN
            IF(IDEBXZ.GE.-3) PRINT *,'XZFZCP. output record length ',
     +         'will be taken from input file (',LRECL,')'
            JRECL = LRECL
         ENDIF
#endif
      ENDIF
 
      LCHOPE   = LENOCC(CHOPE)
      IF(IOPTR.NE.0) CHOPE(LCHOPE:LCHOPE) = ' '
      LCHOPE   = LENOCC(CHOPF) + 1
      IF(IOPTC.NE.0) CHOPE(LCHOPE:LCHOPE) = 'C'
 
#if !defined(CERNLIB_IBMVM)
      IF(IDEBXZ.GE.3) PRINT *,'XZFZCP. call SZOPEN for ',
     +   CHFZOU(1:LCHFZO),JRECL,CHOPE
      CALL SZOPEN(LUNXZO,CHFZOU(1:LCHFZO),JRECL,CHOPE,IRC)
#endif
#if defined(CERNLIB_IBMVM)
      IF(INDEX(OFORM,'Z').EQ.0) THEN
         IF(IDEBXZ.GE.3) PRINT *,'XZFZCP. call SZOPEN for ',
     +      CHFZOU(1:LCHFZO),JRECL,CHOPE
         CALL SZOPEN(LUNXZO,CHFZOU(1:LCHFZO),JRECL,CHOPE,IRC)
      ELSE
         CHTEMP = CHFZOU(1:LCHFZO)
         CALL CTRANS('.',' ',CHTEMP,1,LCHFOU)
         WRITE(CHFILE,9002) LUNXZI,CHTEMP(1:LCHFZI),LBLK
         LCHF = LENOCC(CHFILE)
 
         IF(INDEX(CHFILE(1:LCHF),' ').EQ.
     +      INDEXB(CHFILE(1:LCHF),' ')) THEN
            LCHF = LCHF + 3
            CHFILE(LCHF-2:LCHF-2) = ' '
            CHFILE(LCHF-1:LCHF)   = CHMODE
            CHFILE(LCHF:LCHF)     = '4'
         ENDIF
 
         CALL VMCMS(CHFILE(1:LCHF),IRC)
         IF(IDEBXZ.GE.3) PRINT *,'XZFZCP. rc from ',
     +      CHFILE(1:LCHF),' = ',IRC
         OPEN(LUNXZI,STATUS='NEW',FORM='UNFORMATTED',IOSTAT=IRC)
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
#endif
 
      IF(IRC.EQ.28) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZFZCP. file ',CHFZOU(1:LCHFZO),
     +      ' already exists - specify R option to replace'
         CALL FZENDI(LUNXZI,'TE')
         CLOSE(LUNXZI)
         RETURN
      ENDIF
 
      IF(IDEBXZ.GE.2) PRINT *,'XZFZCP. call FZFILE for ',
     +   'JRECL = ',JRECL,' chopt = ',CHOPF
 
      CALL FZFILE(LUNXZO,JRECL/4,CHOPF)
      CALL FZLOGL(LUNXZO,IDEBXZ)
*
*     Perform the copy
*
    1 CONTINUE
      NUH = 400
      CALL FZIN(LUNXZI,IXDIV,LSUP,JBIAS,' ',NUH,IUHEAD)
      IF((IQUEST(1).LT.0).OR.(IQUEST(1).GE.4)) GOTO 2
      IF(IQUEST(1).EQ.0) IEVENT = IQUEST(11)
*
*     start of run
*
      IF(IQUEST(1).EQ.1) THEN
*        IF(IOPTR.NE.0) GOTO 1
         CALL FZRUN(LUNXZO,IQUEST(11),NUH,IUHEAD)
         GOTO 1
*
*     end of run
*
      ELSEIF(IQUEST(1).EQ.2) THEN
*        IF(IOPTR.NE.0) GOTO 1
         CALL FZRUN(LUNXZO,-1,NUH,IUHEAD)
         GOTO 1
*
*     ZEBRA eof
*
      ELSEIF(IQUEST(1).EQ.3) THEN
      ENDIF
 
      CALL UCOPY(IQUEST(21),IOCR,MIN(IQUEST(20),100))
      CHFZ = 'L'
      IF(IQUEST(14).EQ.0) CHFZ = 'Z'
      CALL FZOUT(LUNXZO,IXDIV,LSUP,IEVENT,CHFZ,IOCR,NUH,IUHEAD)
      GOTO 1
    2 CONTINUE
*
*     Close input and output files
*
      CALL FZENDI(LUNXZI,'TE')
      CALL FZENDO(LUNXZO,'TE')
      CALL SZCLOS(LUNXZI,' ',IRC)
      CALL SZCLOS(LUNXZO,' ',IRC)
 
99    CONTINUE
      END
