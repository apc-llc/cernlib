*
* $Id: xzgetp.F,v 1.4 1998/09/25 09:25:02 mclareni Exp $
*
* $Log: xzgetp.F,v $
* Revision 1.4  1998/09/25 09:25:02  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.3  1997/09/02 08:46:27  mclareni
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
      SUBROUTINE XZGETP(LOCAL,REMOTE,CHOPT,IRC)
#include "cspack/zmach.inc"
#include "cspack/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czbuff.inc"
#include "cspack/czunit.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) LOCAL,REMOTE
#if defined(CERNLIB_IBM)
      CHARACTER*80 CHFILE
#endif
#include "cspack/czoptd.inc"
      CHARACTER*12 NODE
      CHARACTER*8  DELTIM
      CHARACTER*8 CHOPTT
      CHARACTER*4  CHOPI
      DIMENSION IBUFF(8192)
      PARAMETER (NREC=0)
      DIMENSION     IA(512)
      DIMENSION     IB(2)
      DIMENSION     ICONTR(2)
      DIMENSION IX(8)
      DIMENSION MPACK2(2),MPACK9(2)
      DATA MPACK2  / 2, 16 /
      DATA MPACK9  / 9, 3  /
#include "cspack/czoptu.inc"
#include "cspack/czopen.inc"
      IRC = 0
*
*     Open local file, options Output
*
      NCHL = LENOCC(LOCAL)
      NCHR = LENOCC(REMOTE)
      LRECL = 2048
      IF(IOPTA.EQ.0) THEN
      CHOPI = 'NOP'
      IF(IOPTR.NE.0) CHOPI = 'OP'
      LCH = LENOCC(CHOPI) + 1
      IF(IOPTC.NE.0) CHOPI(LCH:LCH) = 'C'
      CALL SZOPEN(1,LOCAL(1:NCHL),LRECL,CHOPI,ISTAT)
         IF(ISTAT.EQ.28.AND.IDEBXZ.GE.-3) PRINT *,'XZGETP. ',
     +      'local file already exists. ',
     +      'Specify option R to replace'
      IF(ISTAT.NE.0) GOTO 95
      ENDIF
 
      NCHO=LENOCC(CHOPT)
      CHOPTT = CHOPT
      IF(NCHO.EQ.0) THEN
         CHOPTT = ' '
         NCHO   = 1
      ENDIF
 
      CHMAIL = 'GETP :'//REMOTE(1:NCHR)//' '//CHOPTT(1:NCHO)
 
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 99
*
  50  CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(CHMAIL.EQ.'KO')GO TO 90
*
*     Transfer data
*
      NR = 0
      NW = 0
      IF(INDEX(CHOPT,'S').NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
      ENDIF
  20  CONTINUE
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) GOTO 97
 
      READ(CHMAIL,'(I3)') LBUF
      IF(LBUF.LT.0) GOTO 40
      NR = NR + 1
      NW = NW + LBUF
 
      ICONTR(1) = 0
      ICONTR(2) = LBUF
 
      CALL CZTCP(IA,ICONTR)
#if defined(CERNLIB_IBM)
      CALL XZATOE(IA(1),8)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_DECS) || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)) || defined(CERNLIB_WINNT)
      IB(1) = IA(3)
      IB(2) = IA(4)
      CALL VXINVB(IB(1),2)
      CALL UPKBYT(IB(1),1,IX(4),4,MPACK9)
      CALL UPKBYT(IB(1),1,IX,4,MPACK2)
      LENX = 1
      IF(IX(5).NE.0) LENX = IX(7) - 3
*
*     Byte swap index vector
*
      CALL VXINVB(IA(3),LENX)
#endif
      CALL UPKBYT(IA(3),1,IX(4),4,MPACK9)
      CALL UPKBYT(IA(3),1,IX,4,MPACK2)
      I = 4
      IF(IX(5).NE.0) THEN
         I = IX(7)
      ENDIF
#if defined(CERNLIB_IBM)
*
*     Translate text
*
      CALL XZATOE(IA(I),(LBUF-I+1)*4)
#endif
      WRITE(1) LBUF,(IA(I),I=1,LBUF)
 
      GOTO 20
*
  40  CONTINUE
*
      CLOSE(1)
      IF(IDEBXZ.GE.0) PRINT *,' File transfer completed'
      IF(INDEX(CHOPT,'S').NE.0)THEN
         CALL CZRTIM(DELTIM)
         CALL TIMED(T)
         READ(DELTIM,'(I2,1X,I2,1X,I2)') IHOUR,IMIN,ISEC
         NSECS = ISEC + IMIN*60 + IHOUR*3600
         IF(NSECS.LE.0) NSECS = 1
         NKILO = NW * IQCHAW / 1024
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
 
 
