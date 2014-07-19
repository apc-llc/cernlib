*
* $Id: pzputr.F,v 1.1.1.1 1996/03/08 15:44:23 mclareni Exp $
*
* $Log: pzputr.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
#include "sys/CERNLIB_machine.h"
#include "_cspack/pilot.h"
      SUBROUTINE PZPUTR(LOCAL,REMOTE,CHOPT,IRC)
#include "czunit.inc"
#include "hcmail.inc"
#include "czsock.inc"
#include "zmach.inc"
#include "pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
C
#include "rzclun.inc"
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
#include "quest.inc"
      CHARACTER*8  CHTAG(100)
      CHARACTER*80 CHFILE
      CHARACTER*90 CHFORM
      CHARACTER*12 CHDATE
      CHARACTER*8  DELTIM
      CHARACTER*1  CHOPE
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
#include "czopts.inc"
#include "czopen.inc"
      IRC = 0
      IF(IDEBXZ.GE.1) PRINT *,'PZPUTR. enter for ',
     +   LOCAL,' ',REMOTE,' ',CHOPT
*
*        Open local RZ file
*
      IQUEST(1)=0
      IOPEN=0
      CHFILE=LOCAL
      NREC=0
      NWKEY=0
      CHFORM = ' '
      LRECL = 0
      IF(IOPTA.EQ.0) THEN
         IF(IOPTC.NE.0) THEN
            CALL RZOPEN(LUNXZI,'RZ',LOCAL,'P',LRECL,ISTAT)
         ELSE
            CALL RZOPEN(LUNXZI,'RZ',LOCAL,' ',LRECL,ISTAT)
         ENDIF
         IF(ISTAT.NE.0) GOTO 95
      ENDIF
      CALL RZFILE(LUNXZI,'RZ',' ')
      IF(IQUEST(1).NE.0)THEN
         CLOSE(LUNXZI)
         GO TO 95
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
      CHMAIL='PPIAF:'//REMOTE(1:NCHR)//' '//CHOPE
*     IF((INDEX(CHOPT,'R').NE.0).OR.(IRELAT.NE.0))
*    +   CHMAIL = CHMAIL(1:LENOCC(CHMAIL)) // 'R '
 
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
 
      IOPEN  = 1
      NREC   = IQ(LCDIR+KQUOTA)
      NWKEY  = IQ(LCDIR+KNWKEY)
      KTAGS  = KKDES+(NWKEY-1)/10+1
      LB     = IQ(LCDIR+KLB)
      LRECL  = IQ(LCDIR+LB+1)
      IDATEC = IQ(LCDIR+KDATEC)
      IDATEM = IQ(LCDIR+KDATEM)
      DO 11 I=1,NWKEY
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
   11 CONTINUE
      CHMAIL=' '
      WRITE(CHMAIL,1000)NWKEY,NREC,LRECL,IDATEC,IDATEM
 1000 FORMAT(5I10)
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
      CALL CZPUTA(CHFORM,ISTAT)
      IF(NWKEY.EQ.0)GO TO 90
      DO 30 I=1,NWKEY
         CHMAIL=CHTAG(I)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
   30 CONTINUE
*
*          Verify that RZ file has been opened by server
*
#include "czmess.inc"
      IF(IRC.NE.0) GOTO 90
*     CALL CZGETA(CHMAIL,ISTAT)
*     IF(ISTAT.NE.0)GO TO 90
*     IF(CHMAIL(1:2).NE.'OK')GO TO 90
 
      IF(INDEX(CHOPT,'S').NE.0) THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
      ENDIF
*
*          Now transfer the file
*
      CALL RZTOFZ(LUNFZO,'C')
      IF(IQUEST(1).NE.0) THEN
         PRINT 2001
 2001 FORMAT(' Error transferring file')
         IRC = 3
      ELSE
         PRINT 2000
 2000 FORMAT(' File transfer completed')
         IF(INDEX(CHOPT,'S').NE.0) THEN
            CALL CZRTIM(DELTIM)
            CALL TIMED(T)
            READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
            NSECS = ISEC + IMIN*60 + IHOUR*3600
            IF(NSECS.LE.0) NSECS = 1
            NKILO = NWORDS*IQCHAW/1024
            RATE  = FLOAT(NKILO)/FLOAT(NSECS)
            NR    = 0
#include "xzstat.inc"
            PRINT *,' Transferred ',NKILO,' KB, rate = ',RATE,' KB/S'
            PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' sec.'
         ENDIF
 
      ENDIF
*
   80 IF(IOPEN.EQ.0)GO TO 99
      CALL RZEND('RZ')
      CLOSE(LUNXZI)
      GO TO 99
*
   90 PRINT *,' Cannot open remote file'
      IRC = 1
      GO TO 80
*
   95 PRINT *,' Cannot open local file'
      IRC = 2
*
   99 END
