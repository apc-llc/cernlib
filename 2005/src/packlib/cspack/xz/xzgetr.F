*
* $Id: xzgetr.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzgetr.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZGETR(LOCAL,REMOTE,CHOPT,IRC)
*              A - local file has already been opened
#include "cspack/czunit.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/quest.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
#include "cspack/rzclun.inc"
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
#if defined(CERNLIB_IBM)
      CHARACTER*80  CHFILE
#endif
      CHARACTER*8   CHTAG(100)
      CHARACTER*90  CHFORM
      CHARACTER*8   DELTIM
      CHARACTER*(*) LOCAL,REMOTE
      CHARACTER*2   CHOPE
      CHARACTER*4   CHOPO
#include "cspack/czopts.inc"
#include "cspack/czopen.inc"
*_______________________________________
*        Send message to remote machine to check if file exists
*        and get file parameters
*
      IF(IDEBXZ.GE.1) PRINT *,'XZGETR. enter for ',
     +   LOCAL,' ',REMOTE,' ',CHOPT
 
      IRC = 0
      CHOPE = ' '
      IF(IOPTN.NE.0) CHOPE = 'N'
      IF(IOPTX.NE.0) CHOPE = 'X'
      LCHOPE = LENOCC(CHOPE) + 1
      IF(IOPTC.NE.0) CHOPE(LCHOPE:LCHOPE) = 'C'
      IF(IOPTE.NE.0) CHOPE(LCHOPE+1:LCHOPE+1) = 'E'
      NCHR  = LENOCC(REMOTE)
      CHMAIL='GETRZ:'//REMOTE(1:NCHR)//' '//CHOPE
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      READ(CHMAIL,1000,ERR=90)NWKEY,NREC,LRECL,IDATEC,IDATEM
 1000 FORMAT(5I10)
      IF(NWKEY.EQ.-1) THEN
*
*     Exchange mode transfer - use XZGETD
*
         CHOPE = ' '
         IF(INDEX(CHOPT,'S').NE.0) CHOPE = 'S'
         IF(IDEBXZ.GE.3)
     +   PRINT *,'XZGETR. Remote file is in exchange format.',
     +           ' Transfer will be performed via XZGETD.'
         CALL XZGETD(LOCAL,REMOTE,LRECL*4,CHOPE,IC)
         GOTO 99
      ENDIF
      CALL CZGETA(CHFORM,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(NWKEY.EQ.0)GO TO 90
      DO 10 I=1,NWKEY
         CALL CZGETA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         CHTAG(I)=CHMAIL
  10  CONTINUE
*
*           Create local file
*
      CHMAIL='OK'
*
*     Open local file using RZOPEN
*
      ISTAT = 0
      IF(IOPTA.EQ.0) THEN
      IF(INDEX(CHOPT,'R').NE.0) THEN
         CHOPO = 'LN'
         IF(IOPTC.NE.0) CHOPO = 'LNP'
      ELSE
         CHOPO = 'N'
         IF(IOPTC.NE.0) CHOPO = 'NP'
      ENDIF
      CALL RZOPEN(LUNXZO,'RZ',LOCAL,CHOPO,LRECL,ISTAT)
      ENDIF
      IF(ISTAT.NE.0)                   GOTO 95
      IF(INDEX(CHOPT,'X').NE.0) THEN
        CALL RZMAKE(LUNXZO,'RZ',NWKEY,CHFORM,CHTAG,MIN(NREC,65000),'X')
      ELSE
        CALL RZMAKE(LUNXZO,'RZ',NWKEY,CHFORM,CHTAG,MIN(NREC,65000),' ')
      ENDIF
      IF(IQUEST(1).NE.0)GO TO 95
*
*           Inform server if file opened on local node
*
  50  CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(CHMAIL.EQ.'KO')GO TO 99
      IF(INDEX(CHOPT,'S').NE.0) THEN
         CALL TIMED(T)
         CALL CZRTIM(DELTIM)
         ENDIF
*
*           Transfer data in RZ file
*
      CALL RZFRFZ(LUNFZI,' ')
      IF(IQUEST(1).NE.0) THEN
      IRC = 3
      PRINT 2001
 2001 FORMAT(' Error transferring file')
      ELSE
#if defined(CERNLIB_NEVER)
*
*     Restore previous modification date
*
      IQ(LTOP+KDATEC)=IDATEC
      IQ(LTOP+KDATEM)=IDATEM
      ISAVE = 2
      CALL SBIT1(IQ(LTOP),2)
*     CALL RZSAVE
#endif
      CALL RZCDIR(' ',' ')
      IF(IDEBXZ.GE.0) PRINT 2000
 2000 FORMAT(' File transfer completed')
      IF(INDEX(CHOPT,'S').NE.0) THEN
            CALL CZRTIM(DELTIM)
            CALL TIMED(T)
            CALL RZSTAT('//RZ',99,' ')
            NKILO = IQUEST(12)*IQCHAW/1024
            READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
            NSECS = ISEC + IMIN*60 + IHOUR*3600
            IF(NSECS.LE.0) NSECS = 1
            RATE  = FLOAT(NKILO)/FLOAT(NSECS)
            NR    = 0
#include "cspack/xzstat.inc"
            PRINT *,' Transferred ',NKILO,' KB, rate = ',RATE,' KB/S'
            PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' sec.'
            ENDIF
 
      ENDIF
 
      IF(INDEX(CHOPT,'L').NE.0) CALL RZLDIR(' ',CHOPT)
      CALL RZEND('RZ')
      CLOSE(LUNXZO)
      GO TO 99
*
  90  PRINT *,' Cannot open remote file'
      IRC = 1
      GO TO 99
*
  95  PRINT *,' Cannot open local file'
      IRC = 2
      CHMAIL='KO'
      GO TO 50
*
  99  END
 
