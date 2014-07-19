*
* $Id: xzputp.F,v 1.4 1998/09/25 09:25:33 mclareni Exp $
*
* $Log: xzputp.F,v $
* Revision 1.4  1998/09/25 09:25:33  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.3  1997/09/02 08:46:28  mclareni
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
      SUBROUTINE XZPUTP(LOCAL,REMOTE,CHOPT,IRC)
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/quest.inc"
#if defined(CERNLIB_IBM)
      CHARACTER*80 CHFILE
#endif
#include "cspack/czoptd.inc"
      DIMENSION     IA(512)
      DIMENSION     IX(8)
      DIMENSION     ICONTR(2)
      DIMENSION     MPACK2(2),MPACK9(2)
      CHARACTER*12  NODE
      CHARACTER*8   DELTIM
      CHARACTER*8   CHOPTT
      CHARACTER*(*) LOCAL,REMOTE
#include "cspack/czunit.inc"
      DATA MPACK2  / 2, 16 /
      DATA MPACK9  / 9, 3  /
#include "cspack/czoptu.inc"
#include "cspack/czopen.inc"
      IRC = 0
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
      IF(IDEBXZ.GE.1) PRINT *,'XZPUTP. enter for ',LOCAL,REMOTE,CHOPT
*
*     Open local file, options Input
*
      LRECL = 2048
      IF(IOPTA.EQ.0) THEN
         IF(IOPTC.EQ.0) THEN
            CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,'IP',ISTAT)
         ELSE
            CALL SZOPEN(LUNXZI,LOCAL(1:NCHL),LRECL,'IPC',ISTAT)
         ENDIF
      IF(ISTAT.NE.0) GOTO 95
      ENDIF
*
*     Send OPEN request to server
*
      NCHO=LENOCC(CHOPT)
      CHOPTT = CHOPT
      IF(NCHO.EQ.0) THEN
         CHOPTT = ' '
         NCHO   = 1
      ENDIF
 
      CHMAIL = 'PUTP :'//REMOTE(1:NCHR)//' '//CHOPTT(1:NCHO)
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
*
*     Verify that PAM file has been opened by server
*
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 90
      IF(CHMAIL(1:2).NE.'OK')GO TO 90
*
*     Start timer
*
      IF(INDEX(CHOPT,'S').NE.0) THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
      ENDIF
*
*     Start transfer
*
      NR   = 0
      NWOR = 0
1     CALL VBLANK(IA,512)
      NUM = 512
      READ(LUNXZI,END=2) NUM,(IA(I),I=1,NUM)
      IF(NUM.EQ.0) GOTO 2
      NWOR = NWOR + NUM
      NR   = NR   + 1
#if defined(CERNLIB_IBM)
*
*     Translate look-ahead name
*
      CALL XZETOA(IA(1),8)
#endif
      CALL UPKBYT(IA(3),1,IX(4),4,MPACK9)
      CALL UPKBYT(IA(3),1,IX,4,MPACK2)
*
*     Loop over body of this record, splitting into lines
*
      I = 4
      LENX = 1
      IF(IX(5).NE.0) THEN
         I = IX(7)
         LENX = IX(7) - 3
      ENDIF
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
*
*     Byte swap index vector
*
      CALL VXINVB(IA(3),LENX)
#endif
*
*     Send #words to read
*
      WRITE(CHMAIL,'(I3)') NUM
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 97
#if defined(CERNLIB_IBM)
*
*     Translate text
*
      CALL XZETOA(IA(I),(NUM-I+1)*4)
#endif
*
*     Send data to server
*
      LBUF      = NUM
      ICONTR(1) = 1
      ICONTR(2) = LBUF
      CALL CZTCP(IA,ICONTR)
 
10    CONTINUE
*
*     Look for null byte in text
*
      IF(IDEBXZ.LT.3) GOTO 1
      J = LOCBYT(0,IA(I),20,1,1,8)
      IF(J.EQ.0) J=20
      PRINT 9001,(IA(K),K=I,I+J-1)
9001  FORMAT(1X,20A4/)
      I = I + J
      IF(I.LT.NUM) GOTO 10
      GOTO 1
*
2     CONTINUE
 
      NUM = -1
      WRITE(CHMAIL,'(I3)') NUM
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 97
 
      CLOSE(LUNXZI)
      IF(IDEBXZ.GE.0) PRINT *,' File transfer completed '
      IF(INDEX(CHOPT,'S').NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         NKILO = NWOR*IQCHAW / 1024
         RATE  = FLOAT(NKILO)/FLOAT(NSECS)
#include "cspack/xzstat.inc"
         PRINT *,' Transferred ',NR,' records, transfer rate = ',RATE,
     +           ' KB/S'
         PRINT *,' Elapsed time = ',DELTIM,' CP time = ',T,' sec.'
      ENDIF
      GO TO 99
*
*          Error
*
  90  PRINT *, ' Cannot open remote file'
      IRC = 1
      GO TO 99
  95  PRINT *, ' Cannot open local file'
      IRC = 2
      GO TO 99
  97  PRINT *, 'Problem in transferring file'
      IRC = 3
      CLOSE(1)
*
  99  LBUF = 270
      END
 
 
