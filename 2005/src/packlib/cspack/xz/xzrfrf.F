*
* $Id: xzrfrf.F,v 1.5 1998/09/25 09:25:36 mclareni Exp $
*
* $Log: xzrfrf.F,v $
* Revision 1.5  1998/09/25 09:25:36  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.4  1997/10/23 13:26:28  mclareni
* NT mods
*
* Revision 1.3  1997/09/02 08:46:28  mclareni
* WINNT mods, mostly cpp defines
* call vxinvb for WINNT too.
*
* Revision 1.2  1997/01/17 08:56:18  gunter
* call vxinvb for linux too.
*
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZRFRF(CHFZ,CHRZ,LRECL,CHOPT,IRC)
 
      CHARACTER*(*) CHRZ,CHFZ
      CHARACTER*4   CHOPE,CHOPF,CHOPR
 
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
#include "zebra/rzk.inc"
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
      CHARACTER*8  CHTAG(100)
      CHARACTER*99 CHFORM
      CHARACTER*12 CHORG
      DIMENSION IHTAG(2),ICDIR(400),IHEAD(500)
      EQUIVALENCE (ICDIR(1),IHEAD(4))
#include "cspack/quest.inc"
      CHARACTER*80 CARD
      CHARACTER*8  DELTIM
      DIMENSION    IUHEAD(400)
      DIMENSION    IOCR(100)
      PARAMETER (JBIAS=2)
      PARAMETER (MEGA=1024*1024)
      COMMON/FZSTAT/INFLUN,INFSTA,INFOFZ(40)
      INTEGER      OURECL
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
 
      IRC    = 0
      LCHRZ  = LENOCC(CHRZ)
      LCHFZ  = LENOCC(CHFZ)
      LCHOPT = LENOCC(CHOPT)
      NRECQQ = IQUEST(10)
 
      IF(LCHRZ.EQ.0.OR.LCHFZ.EQ.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRFRF. error - input or ',
     +      'output file name missing'
         IRC = -1
         GOTO 99
      ENDIF
 
      IF(IDEBXZ.GE.1) PRINT *,'XZRFRF. enter for ',CHFZ(1:LCHFZ),
     +   ' ',CHRZ(1:LCHRZ),' ',CHOPT(1:LCHOPT)
 
      CHOPE = ' '
      CHOPF = 'XI'
      JRECL = 80
 
#if defined(CERNLIB_UNIX)
      CHOPE = 'D'
      CHOPF = 'DI'
#endif
 
      IF(IOPTA.NE.0) THEN
         CHOPE = 'F'
         CHOPF = 'AI'
      ELSE
#if defined(CERNLIB_VAXVMS)
         INQUIRE(FILE=CHFZ(1:LCHFZ),RECL=JRECL)
#endif
      ENDIF
 
      LCHOPE   = LENOCC(CHOPE) + 1
      IF(IOPTC.NE.0) CHOPF(LCHOPE:LCHOPE) = 'C'
 
      CALL SZOPEN(LUNXZI,CHFZ(1:LCHFZ),JRECL,CHOPE,IRC)
      IF(IRC.NE.0) GOTO 99
 
      IF(IOPTA.EQ.0) THEN
*
*     Get record length from file
*
#if !defined(CERNLIB_UNIX)
         READ(LUNXZI,IOSTAT=ISTAT) ITEST
         REWIND(LUNXZI)
#endif
#if defined(CERNLIB_UNIX)
         READ(LUNXZI,REC=1,IOSTAT=ISTAT) ITEST
#endif
 
         IF(ISTAT.NE.0) THEN
            PRINT *,'XZRFRF. error ',ISTAT,' reading input file'
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
         PRINT *,'XZRFRF. cannot determine record length of ',
     +      'input file. How was this file created?'
         IRC = JRECL
         CLOSE(LUNXZI)
         GOTO 99
      ENDIF
 
      IF(IDEBXZ.GE.2) PRINT *,'XZRFRF. record length of input ',
     +   'file is ',JRECL,' bytes'
*
*     Close and reopen file
*
         CALL SZCLOS(LUNXZI,' ',IRC)
         CALL SZOPEN(LUNXZI,CHFZ(1:LCHFZ),JRECL,CHOPE,IRC)
 
      ENDIF
 
      IF(IDEBXZ.GE.2) PRINT *,'XZRFRF. call FZFILE for ',
     +   'JRECL = ',JRECL,' chopt = ',CHOPF
      CALL FZFILE(LUNXZI,JRECL/4,CHOPF)
      CALL FZLOGL(LUNXZI,IDEBXZ)
 
   5  NH=KNSIZE
      CALL FZIN(LUNXZI,0,0,0,'S',NH,IHEAD)
      IRC = IQUEST(1)
      IF(IRC.NE.0) THEN
        IF(IDEBXZ.GE.0) PRINT *,'XZRFRF. error reading FZ file'
        GO TO 98
      ENDIF
      IF(NH.NE.2)           GO TO 5
      IF(IHEAD(1).NE.12345) GO TO 5
   7  NH=KNSIZE
      CALL FZIN(LUNXZI,0,0,0,'S',NH,IHEAD)
      IRC = IQUEST(1)
      IF(IRC.NE.0) THEN
        IF(IDEBXZ.GE.0) PRINT *,'XZRFRF. error reading FZ file'
        GO TO 98
      ENDIF
      IF(NH.NE.KNSIZE)  GO TO 7
      IF(IHEAD(1).NE.1) GO TO 7
C
      NREC   = ICDIR(KQUOTA)
      IF(IOPTQ.NE.0) THEN
         IF(IDEBXZ.GE.0) WRITE(6,7001) NREC,NRECQQ
7001  FORMAT(' XZRFRF. quota of output file will be changed from ',
     +       I10,' to ',I10)
         NREC = NRECQQ
      ENDIF
      NWKEY  = ICDIR(KNWKEY)
      KTAGS  = KKDES+(NWKEY-1)/10+1
      CHFORM = ' '
      LB     = ICDIR(KLB)
      OURECL = ICDIR(LB+1)
 
      DO 10 I=1,NWKEY
         CALL ZITOH(ICDIR(KTAGS+2*I-2),IHTAG,2)
         CALL UHTOC(IHTAG,4,CHTAG(I),8)
         IKDES=(I-1)/10
         IKBIT1=3*I-30*IKDES-2
         IFORM=JBYT(ICDIR(KKDES+IKDES),IKBIT1,3)
         IF(IFORM.EQ.3)THEN
            CHFORM(I:I)='H'
         ELSEIF(IFORM.EQ.4)THEN
            CHFORM(I:I)='A'
         ELSEIF(IFORM.EQ.1)THEN
            CHFORM(I:I)='B'
         ELSE
            CHFORM(I:I)='I'
         ENDIF
  10  CONTINUE
 
      CALL FZENDI(LUNXZI,'IQ')
*
*     Open output file
*
      IF(LRECL.GT.0) OURECL = LRECL / IQCHAW
      CHOPR = 'N'
      IF(IOPTX.NE.0) CHOPR = 'XN'
      LCHOPR = LENOCC(CHOPR) + 1
      IF(IOPTC.NE.0) CHOPR(LCHOPR:LCHOPR) = 'P'
 
      IF(IDEBXZ.GE.1) PRINT 9001,CHRZ(1:LCHRZ),CHOPR,OURECL
9001  FORMAT(' XZRFRF. calling RZOPEN for ',A,1X,A,1X,I6)
 
      CALL RZOPEN(LUNXZO,'RZ',CHRZ(1:LCHRZ),CHOPR,OURECL,IRC)
 
      CHOPR  = ' '
      LCHOPR = 0
      IF(IOPTX.NE.0) THEN
         CHOPR  = 'X'
         LCHOPR = 1
      ENDIF
      IF(IOPTN.NE.0) CHOPR(LCHOPR+1:) = 'N'
 
      IF(IDEBXZ.GE.1) PRINT 9002,CHOPR,NREC
9002  FORMAT(' XZRFRF. calling RZMAKE for ',A,1X,I6)
 
      CALL RZMAKE(LUNXZO,'RZ',NWKEY,CHFORM,CHTAG,NREC,CHOPR)
      IQ(LTOP+KDATEC)=ICDIR(KDATEC)
      IQ(LTOP+KDATEM)=ICDIR(KDATEM)
      CALL RZFRFZ(LUNXZI,' ')
      IF(IOPTS.NE.0) CALL RZSTAT('//RZ',99,' ')
 
      CALL RZCLOS('RZ',' ')
98    CONTINUE
      CALL FZENDI(LUNXZI,'TE')
      CALL SZCLOS(LUNXZI,' ',IRC)
 
99    CONTINUE
 
      END
