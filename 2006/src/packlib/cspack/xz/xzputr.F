*
* $Id: xzputr.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzputr.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZPUTR(LOCAL,REMOTE,CHOPT,IRC)
#include "cspack/czunit.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
C
#include "cspack/rzclun.inc"
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
#include "cspack/quest.inc"
      CHARACTER*8  CHTAG(100)
      CHARACTER*80 CHFILE
      CHARACTER*90 CHFORM
      CHARACTER*12 CHDATE
      CHARACTER*8  DELTIM
      CHARACTER*1  CHOPE
      CHARACTER*4  CHOPRZ
      DIMENSION IHTAG(2)
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
      CHARACTER*(*) LOCAL,REMOTE
      DIMENSION ITEST(50)
*
*_______________________________________
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
      IRC = 0
 
      IF(IDEBXZ.GE.1) PRINT *,'XZPUTR. enter for ',
     +   LOCAL,' ',REMOTE,' ',CHOPT
*
*        Open local RZ file
*
      IQUEST(1) = 0
      IOPEN     = 0
      CHFILE    = LOCAL
      NREC      = 0
      NWKEY     = 0
      CHFORM    = ' '
      LRECL     = 0
 
      IF(IOPTA.EQ.0) THEN
         CHOPRZ = ' '
         LCHPRZ = 0
         IF(IOPTE.NE.0) THEN
            CHOPRZ = 'X'
            LCHPRZ = 1
         ENDIF
 
         IF(IOPTC.NE.0) THEN
            LCHPRZ = LCHPRZ + 1
            CHOPRZ(LCHPRZ:LCHPRZ) = 'P'
         ENDIF
 
         CALL RZOPEN(LUNXZI,'RZ',LOCAL,CHOPRZ,LRECL,ISTAT)
         IF(ISTAT.NE.0) GOTO 50
      ENDIF
 
      CHOPRZ = ' '
      IF(IQUEST(12).NE.0) CHOPRZ = 'X'
 
      CALL RZFILE(LUNXZI,'RZ',CHOPRZ)
      IF(IQUEST(1).NE.0)THEN
         CLOSE(LUNXZI)
         GOTO 50
      ENDIF
 
      IF(INDEX(CHOPT,'S').NE.0) THEN
         CALL RZSTAT('//RZ',99,' ')
         NWORDS = IQUEST(12)
      ENDIF
 
      IF(INDEX(CHOPT,'L').NE.0) CALL RZLDIR(' ',CHOPT)
*
*        Send message to remote machine with the file parameters
*
      CHOPE = ' '
      IF(INDEX(CHOPT,'N').NE.0) CHOPE = 'N'
      IF(INDEX(CHOPT,'X').NE.0) CHOPE = 'X'
      LCHOPE = LENOCC(CHOPE)
      IF(IOPTC.NE.0) THEN
         CHOPE(LCHOPE+1:LCHOPE+1) = 'C'
         LCHOPE = LCHOPE + 1
      ENDIF
      IF(INDEX(CHOPT,'R').NE.0.OR.IRELAT.NE.0)
     +               CHOPE(LCHOPE+1:LCHOPE+1) = 'L'
 
      NCHR = LENOCC(REMOTE)
      CHMAIL='PUTRZ:'//REMOTE(1:NCHR)//' '//CHOPE
*     IF((INDEX(CHOPT,'R').NE.0).OR.(IRELAT.NE.0))
*    +   CHMAIL = CHMAIL(1:LENOCC(CHMAIL)) // 'R '
 
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GOTO 99
 
      IOPEN  = 1
      NREC   = IQ(LCDIR+KQUOTA)
      NWKEY  = IQ(LCDIR+KNWKEY)
      KTAGS  = KKDES+(NWKEY-1)/10+1
      LB     = IQ(LCDIR+KLB)
      LRECL  = IQ(LCDIR+LB+1)
      IDATEC = IQ(LCDIR+KDATEC)
      IDATEM = IQ(LCDIR+KDATEM)
*
*     Test if this file is in exchange mode
*
*     CALL RZIODO(LUNXZI,50,2,ITEST,1)
*SELF,IF=VAX,DECS.
*
*     CALL VXINVB(ITEST(9),1)
*     IMODEX = JBIT(ITEST(9),12)
*     IMODEX = IOR(ITEST(6),0)
*SELF,IF=-VAX,IF=-DECS.
*     IMODEX = ITEST(6)
*SELF.
*
#if defined(CERNLIB_UNIX)
      IF(INDEX(CHOPT,'X').NE.0) THEN
#endif
#if !defined(CERNLIB_UNIX)
      IF((IMODEX.NE.0).AND.(INDEX(CHOPT,'N').EQ.0)) THEN
#endif
*
*     Set NWKEY to indicate that this is an exchange mode transfer
*
         NWKEY = -1
         WRITE(CHMAIL,9002)NWKEY,NREC,LRECL,IDATEC,IDATEM
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GOTO 40
*
*     Now transfer the file using XZPUTD
*
         CHOPE = ' '
         IF(INDEX(CHOPT,'S').NE.0) CHOPE = 'S'
         CALL XZPUTD(LOCAL,REMOTE,LRECL*4,CHOPE,IC)
         CALL RZEND('RZ')
         GOTO 99
      ELSE
         DO 10 I=1,NWKEY
            CALL ZITOH(IQ(LCDIR+KTAGS+2*I-2),IHTAG,2)
            CALL UHTOC(IHTAG,4,CHTAG(I),8)
            IKDES=(I-1)/10
            IKBIT1=3*I-30*IKDES-2
            IFORM=JBYT(IQ(LCDIR+KKDES+IKDES),IKBIT1,3)
            IF(IFORM.EQ.3)THEN
               CHFORM(I:I)='H'
            ELSEIF(IFORM.EQ.4) THEN
               CHFORM(I:I)='A'
            ELSEIF(IFORM.EQ.1) THEN
               CHFORM(I:I)='B'
            ELSE
               CHFORM(I:I)='I'
            ENDIF
   10    CONTINUE
         CHMAIL=' '
         WRITE(CHMAIL,9002)NWKEY,NREC,LRECL,IDATEC,IDATEM
 9002 FORMAT(5I10)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GOTO 40
         CALL CZPUTA(CHFORM,ISTAT)
         IF(NWKEY.EQ.0)GOTO 40
         DO 20 I=1,NWKEY
            CHMAIL=CHTAG(I)
            CALL CZPUTA(CHMAIL,ISTAT)
            IF(ISTAT.NE.0)GOTO 40
   20    CONTINUE
*
*          Verify that RZ file has been opened by server
*
         CALL CZGETA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GOTO 40
         IF(CHMAIL(1:2).NE.'OK')GOTO 40
 
         IF(INDEX(CHOPT,'S').NE.0) THEN
            CALL CZRTIM(DELTIM)
            CALL TIMED(T)
         ENDIF
 
*
*          Now transfer the file
*
         CALL RZTOFZ(LUNFZO,'C')
         IF(IQUEST(1).NE.0) THEN
            PRINT 9003
 9003 FORMAT(' Error transferring file')
            IRC = 3
         ELSE
            IF(IDEBXZ.GE.0) PRINT 9004
 9004 FORMAT(' File transfer completed')
            IF(INDEX(CHOPT,'S').NE.0) THEN
               CALL CZRTIM(DELTIM)
               CALL TIMED(T)
               READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
               NSECS = ISEC + IMIN*60 + IHOUR*3600
               IF(NSECS.LE.0) NSECS = 1
               NKILO = NWORDS*IQCHAW/1024
               RATE = FLOAT(NKILO)/FLOAT(NSECS)
               NR = 0
#include "cspack/xzstat.inc"
               PRINT *,' Transferred ',NKILO,' KB, rate = ',RATE,' KB/'
     +         //'S'
               PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' '
     +         //'sec.'
            ENDIF
 
         ENDIF
      ENDIF
*
   30 IF(IOPEN.EQ.0)GOTO 99
      CALL RZEND('RZ')
      CLOSE(LUNXZI)
      GOTO 99
*
   40 PRINT *,' Cannot open remote file'
      IRC = 1
      GOTO 30
*
   50 PRINT *,' Cannot open local file'
      IRC = 2
*
  99  END
