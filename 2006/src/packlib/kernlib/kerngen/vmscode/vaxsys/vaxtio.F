*
* $Id: vaxtio.F,v 1.1.1.1 1996/02/15 17:50:26 mclareni Exp $
*
* $Log: vaxtio.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:26  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE VAXTIO (LUNP,MODOPP,IBUF,NDOP,NDONEP,NCODEP,LUNMSGP)
C
C CERN PROGLIB# Z301    VAXTIO          .VERSION KERNVAX  2.04  830531
C ORIG. 11/05/83 G.CIAPETTI + J.ZOLL
C
C-    HANDLE TAPES MOUNTED 'FOREIGN' WITH DIRECT QIO SYSTEM REQUESTS
C

C----    PARAMETERS ON INPUT

C        LUN :  LOGICAL UNIT NUMBER POINTING TO THE FILE
C               SPECIFIED BY THE LOGICAL NAME 'QIOUNIT<LUN>'
C               LUN MUST BE   0 < LUN < 61
C      MODOP :  OPERATION MODE REQUEST
C               =  -2  WRITE EOF
C                  -1  WRITE NDO BYTES FROM IBUF TO TAPE
C                   0  READ AT MOST NDO BYTES FROM TAPE TO IBUF
C                   1  ASSIGN CHANNEL
C                   2  SKIP NDO RECORDS, BACKWARDS IF NDO -VE
C                   3  SKIP NDO FILES,   BACKWARDS IF NDO -VE
C                   4  REWIND
C                   5  REWIND/UNLOAD
C                   6  DE-ASSIGN CHANNEL
C
C       IBUF :  DATA AREA FOR THE RECORD OF NDO BYTES
C       NDO  :  NUMBER OF UNITS TO BE DONE
C     LUNMSG :  LOGICAL UNIT TO RECEIVE MESSAGES
C               = 0 : MESSAGES SUPPRESSED

C----    PARAMETERS ON OUTPUT

C      NDONE : NUMBER OF UNITS DONE
C              READ : +VE  NDONE BYTES READ INTO IBUF
C                       0  END OF FILE,TAPE,VOLUME
C                     -VE  ERROR
C
C              WRITE: +VE  NDONE BYTES WRITTEN
C                       0  END OF TAPE/VOLUME, RECORD WRITTEN + EOF
C                     -VE  ERROR
C
C              WEOF :   1  THREE EOF WRITTEN, 2 BACKSPACED
C                       0  DITTO, BUT END-OF-TAPE
C                     -VE  ERROR
C
C              SKIP : +VE  NDONE RECORDS/FILES SKIPPED
C                     -VE  ERROR
C
C              ERRORS :
C                      -1  READ ERROR
C                      -7  CATASTROPHIC ERROR,
C                          DO NOT ATTEMPT FURTHER READS
C
C      NCODE :  OPERATION STATUS CODE RETURNED FROM QIO
C               = 1 IF ALL OK

C   THIS ROUTINE USES EVENT FLAGS 1 AND 2, ALSO 0 WITH WAIT_2SEC
C----

      DIMENSION    LUNP(9),MODOPP(1),IBUF(999),NDOP(9)
     +,            NDONEP(9),NCODEP(9),LUNMSGP(9)
      DIMENSION    MM(68)

      INTEGER*2    JCHANV,LUNFLV,IOSB2V(240), JCHAN,LUNFL

      COMMON /VAXTIOC/ JCHANV(60), LUNFLV(60), IOSB4V(120)

      EQUIVALENCE  (IOSB4V,IOSB2V)
      DATA         JCHANV /60*0/,  LUNFLV /60*-1/, IOSB2V / 240*1 /

      INTEGER      SYS$QIO, SYS$QIOW
     +,            SYS$TRNLOG, SYS$ASSIGN, SYS$DASSGN

      EXTERNAL     IO$_READLBLK,IO$_WRITELBLK,IO$_WRITEOF
     +,            IO$_SKIPRECORD,IO$_SKIPFILE
     +,            IO$_REWIND,IO$_REWINDOFF

      EXTERNAL     SS$_ENDOFFILE, SS$_ENDOFTAPE, SS$_ENDOFVOLUME
     +,            SS$_DATAOVERUN,SS$_PARITY

      CHARACTER    MESSAGE*256
      CHARACTER    IONAME*64
      CHARACTER*8  RECFIL(2)
      DATA         RECFIL/'RECORDS ','FILES   '/

      DATA  LIMIT  / 64 /


C--                COPY PARAMETERS TO LOCAL

      LUN    = LUNP(1)
      MODOP  = MODOPP(1)
      LUNMSG = LUNMSGP(1)

      IF (LUN.LE.0)          GO TO 93
      IF (LUN.GE.61)         GO TO 93
      JSB4   = 2*LUN  - 1
      JSB2   = 2*JSB4 - 1

   10 NDO    = NDOP(1)
      NDONE  = NDO
      ISTAT  = 0
      LUNFL  = LUNFLV(LUN)
      JCHAN  = JCHANV(LUN)
      IF   (LUNFL)                           31,61,11
   11 IF   (MODOP)                           21,12,40

C----              READ REQUEST

   12 MODE  = %LOC(IO$_READLBLK)
      ISTAT = SYS$QIOW (%VAL(1),%VAL(JCHAN),%VAL(MODE)
     +,                 IOSB4V(JSB4),,, IBUF,%VAL(NDO),,,,)

      IF (.NOT.ISTAT)                        GO TO 71
   16 ISTAT = IOSB2V(JSB2)
      IF (ISTAT.EQ.0)        GO TO 16

      IF (ISTAT.EQ.%LOC(SS$_DATAOVERUN))     GO TO 18
      IF (.NOT.ISTAT)                        GO TO 71
      NDONE = JBYT (IOSB4V(JSB4),17,16)

   18 LUNFLV(LUN) = 11
   19 NDONEP(1)   = NDONE
      NCODEP(1)   = ISTAT
      RETURN

C----              WRITE REQUEST

   21 NSTATW = 0
      IF   (MODOP+2)                         93,25,22
   22 MODE  = %LOC(IO$_WRITELBLK)
      ISTAT = SYS$QIOW (%VAL(1),%VAL(JCHAN),%VAL(MODE)
     +,                 IOSB4V(JSB4),,, IBUF,%VAL(NDO),,,,)

      IF (.NOT.ISTAT)                        GO TO 71
   24 ISTAT = IOSB2V(JSB2)
      IF (ISTAT.EQ.0)        GO TO 24

      IF (ISTAT.EQ.%LOC(SS$_ENDOFTAPE))      GO TO 70
      IF (ISTAT.EQ.%LOC(SS$_ENDOFVOLUME))    GO TO 70
      IF (.NOT.ISTAT)                        GO TO 71
      NDONE = JBYT (IOSB4V(JSB4),17,16)
      GO TO 18

C--                WRITE 3 EOF

   25 MODE  = %LOC(IO$_WRITEOF)
      NLOOP = 5
      NDONE = 0

      DO 26  J=1,3
      ISTAT = SYS$QIOW (%VAL(1),%VAL(JCHAN),%VAL(MODE)
     +,                 IOSB4V(JSB4),,, ,,,,,)

      IF (ISTAT)    ISTAT = IOSB2V(JSB2)

      IF (NSTATW.NE.0)                       GO TO 26
      IF (ISTAT.EQ.%LOC(SS$_ENDOFTAPE))      GO TO 70
      IF (ISTAT.EQ.%LOC(SS$_ENDOFVOLUME))    GO TO 70
      IF (.NOT.ISTAT)                        GO TO 71
      NLOOP = 6
      NDONE = 1
   26 CONTINUE

C--                BACKSPACE OVER 2 OR 3 EOF

      IF (NSTATW.NE.0)  ISTAT=NSTATW
      JFORW = 5
      GO TO 82

C----              GET CHANNEL ON FIRST CONTACT

   31 IF (MODOP.EQ.6)                        GO TO 37
      CALL UCTOH1 ('QIOUNIT00',MM,9)
      CALL USET   (LUN,MM,8,9)
      LGMSG = 9
      N     = 0

C--                CLEAR MULTI-LEVEL LOGICAL NAME

   32 LGNAM = LGMSG
      CALL UH1TOC (MM,IONAME,LGNAM)
      IF (N.GE.12)           GO TO 33
      ISTAT = SYS$TRNLOG (IONAME(1:LGNAM),LGMSG,MESSAGE,,,)
      IF (ISTAT.NE.1)        GO TO 33
      IF (LGMSG.EQ.0)        GO TO 33
      IF (LGMSG.GE.64)       GO TO 33
      CALL UCTOH1 (MESSAGE,MM,LGMSG)
      N = N + 1
      GO TO 32

   33 IF (LUNMSG.EQ.0)       GO TO 34
      N = MIN0 (LGNAM,50)
      WRITE (LUNMSG,9033) LUN, IONAME(1:N)
 9033 FORMAT ('0VAXTIO.',12X,'LUN=',I2,' ASSIGN ',A)

   34 ISTAT = SYS$ASSIGN (IONAME(1:LGNAM),JCHAN,,)
      IF (.NOT.ISTAT)                        GO TO 71
      JCHANV(LUN) = JCHAN
      LUNFLV(LUN) = 11
      IF (MODOP.NE.1)                        GO TO 10
   37 NDONE = 1
      GO TO 19

C---------         SPECIAL OPERATIONS

   40 IF (MODOP.GE.7)                        GO TO 93
      GO TO (41,42,43,51,52,57), MODOP

C--                ASSIGN CHANNEL (WAS ALREADY ASSIGNED!)

   41 NDONE = 0
      GO TO 19

C--                SKIP NDO  RECORDS

   42 MODE = %LOC(IO$_SKIPRECORD)
      GO TO 44

C--                SKIP NDO  FILES

   43 MODE = %LOC(IO$_SKIPFILE)
   44 IF (LUNMSG.EQ.0)                       GO TO 45
      WRITE (LUNMSG,9044) LUN,NDO,RECFIL(MODOP-1)
 9044 FORMAT (' VAXTIO.',12X,'LUN=',I2,' GO TO SKIP',I6,1X,A)

   45 ISTAT = SYS$QIOW (%VAL(1),%VAL(JCHAN),%VAL(MODE)
     +,                 IOSB4V(JSB4),,, %VAL(NDO),,,,,)

      IF (.NOT.ISTAT)                        GO TO 71
   46 ISTAT = IOSB2V(JSB2)
      IF (ISTAT.NE.0)                        GO TO 47
      CALL WAIT_2SEC
      GO TO 46

   47 IF (.NOT.ISTAT)                        GO TO 71
   48 NDONE = JBYT (IOSB4V(JSB4),17,16)
      IF (LUNMSG.EQ.0)                       GO TO 18

      WRITE (LUNMSG,9045) NDONE
 9045 FORMAT (37X,I6,' DONE.')
      GO TO 18

C--                REWIND

   51 MODE = %LOC(IO$_REWIND)
      GO TO 53

C--                REWIND/UNLOAD

   52 MODE = %LOC(IO$_REWINDOFF)
   53 CONTINUE

      ISTAT = SYS$QIO (%VAL(2),%VAL(JCHAN),%VAL(MODE)
     +,                IOSB4V(JSB4),,, ,,,,,)

      IF (.NOT.ISTAT)        GO TO 71
      LUNFLV(LUN) = 0
      NDONE = 1
      GO TO 19

C--                DE-ASSIGN CHANNEL

   57 ISTAT = SYS$DASSGN (%VAL(JCHAN))
      IF (.NOT.ISTAT)        GO TO 71
      LUNFLV(LUN) = -1
      NDONE = 1
      GO TO 19

C----              WAIT FOR REWIND COMPLETE

   60 CALL WAIT_2SEC
   61 IF (IOSB4V(JSB4).EQ.0)    GO TO 60
      LUNFLV(LUN) = 11
      GO TO 10

C--------          ERROR HANDLING

   70 NSTATW = ISTAT

   71 IF (LUNMSG.EQ.0)                       GO TO 74
      WRITE (LUNMSG,9071) LUN,JCHAN,ISTAT
 9071 FORMAT ('0VAXTIO.',12X,'LUN=',I3,' CHANNEL = ',Z4
     +,' ERROR NO.= ',Z4,' HEX')

      CALL SYS$GETMSG (%VAL(ISTAT),LGMSG,MESSAGE,%VAL(15),)
      LGMSG = MIN0 (LGMSG,60)
      WRITE (LUNMSG,9073) MESSAGE(1:LGMSG)
 9073 FORMAT (20X,A)

C----              ASSIGNMENT ERRORS

   74 IF (LUNFL.GE.0)                        GO TO 75
      IF (LUNFL+LIMIT.LT.0)                  GO TO 88
      LUNFLV(LUN) = LUNFL - 1
      NDONE = -7
      GO TO 19

C----              NON-ASSIGNMENT ERRORS

   75 NDONE = -1
      IF (LUNFL.GE.LIMIT)                    GO TO 88
      LUNFLV(LUN) = LUNFL + 1

C--                READ ERRORS

      IF (MODOP.LT.0)                        GO TO 81
      IF (MODOP.GE.4)                        GO TO 86
      IF (ISTAT.EQ.%LOC(SS$_ENDOFFILE))      GO TO 78
      IF (ISTAT.EQ.%LOC(SS$_ENDOFTAPE))      GO TO 76
      IF (ISTAT.NE.%LOC(SS$_ENDOFVOLUME))    GO TO 79

   76 IF (MODOP.NE.0)                        GO TO 77
      IF (LUNFL.GE.11)  LUNFL=1
      IF (LUNFL.GE.8)             GO TO 77
      LUNFLV(LUN) = LUNFL
      NDONE = JBYT (IOSB4V(JSB4),17,16)
      GO TO 19

   77 MODE = %LOC(IO$_SKIPRECORD)
      CALL SYS$QIOW (%VAL(1),%VAL(JCHAN),%VAL(MODE)
     +,              ,,, %VAL(-1),,,,,)
   78 IF (MODOP.NE.0)                        GO TO 48
      NDONE = 0
      GO TO 19

   79 IF (ISTAT.EQ.%LOC(SS$_PARITY))         GO TO 19
      GO TO 87

C--                WRITE ERRORS

   81 IF (NSTATW.EQ.0)                       GO TO 87
      IF (MODOP.NE.-2)                       GO TO 25
      NLOOP = 3
      JFORW = 3

C--                E-O-T ON WRITE, BACKSPACE

   82 MODE = %LOC(IO$_SKIPRECORD)
      N    = -1
      DO 83  J=1,NLOOP
      IF (J.EQ.JFORW)  N=1
      CALL SYS$QIOW (%VAL(1),%VAL(JCHAN),%VAL(MODE)
     +,              ,,, %VAL(N),,,,,)
   83 CONTINUE
      IF (NLOOP.EQ.3)                        GO TO 25
      GO TO 19

C----              CATASTROPHIC ERRORS

   86 CONTINUE

   87 NDONE = -7
      GO TO 19

C----              TOO MANY CONSECUTIVE ERRORS

   88 IF (LUNMSG.EQ.0)                       GO TO 99
      WRITE (LUNMSG,9088) LUN
 9088 FORMAT ('0VAXTIO.',12X,'LUN=',I3,' ABEND FOR TOO MANY ERRORS.')
      GO TO 99

C--                FAULTY REQUEST CODE

   93 WRITE (LUNMSG,9093) LUN,MODOP
 9093 FORMAT ('0VAXTIO.',12X,' ABEND FOR FAULTY LUN/MODOP = ',2I6)
   99 CALL ABEND
C  99 NDONEP(1) = -7
C     RETURN
      END
