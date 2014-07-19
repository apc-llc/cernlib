*
* $Id: epblin.F,v 1.1.1.1 1996/03/08 15:21:43 mclareni Exp $
*
* $Log: epblin.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:43  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPBLIN(IBUF,IERR)
CD    COMMON/EPDBGC/DBUGFL
CD    LOGICAL DBUGFL
#include "epio/epiocom.inc"
      DIMENSION ILBHD(16)
#include "epio/wordsize.inc"
#if defined(CERNLIB_CDC)||defined(CERNLIB_IBM)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_NORD)||defined(CERNLIB_CRAY)||defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      DIMENSION IBUF(1)
#endif
#if defined(CERNLIB_VAX)
      INTEGER*2 IBUF(1)
      INTEGER*4 EPDKRECL
      INCLUDE 'SYS$LIBRARY:FORIOSDEF/NOLIST'
      EXTERNAL SS$_NORMAL,SS$_ENDOFFILE,SS$_ENDOFTAPE
      EXTERNAL SS$_PARITY,SS$_DATAOVERUN,EPDKRECL
#endif
#if defined(CERNLIB_APOLLO)
%INCLUDE '/sys/ins/base.ins.ftn'
%INCLUDE '/sys/ins/error.ins.ftn'
%INCLUDE '/sys/ins/streams.ins.ftn'
      INTEGER*2 IBUF(1), ERROR_$RETURNED
      INTEGER STATUS_$RETURNED
#endif
#if defined(CERNLIB_UNIVAC)
      DIMENSION NACC(2)
      DATA IEOF/'EOFM'/,IEP/O000000105120/
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_QMIBMFSI))
      CHARACTER*4 ULP
      DATA ULP/'ULP '/
#endif
#if defined(CERNLIB_QMIBMFSI)
      CHARACTER*4 ULP
      DATA ULP/ZE4D3D740/
#endif
CD    LABEL=1001
CD    IF(DBUGFL)  PRINT*,LABEL,ISTART
      LUN=LIST(ISTART+10)
C--SET NP (PHYS BLOCK LENGTH) = 0  AND NEW FORMAT INDICATOR
      LIST(ISTART+14)=0
      IERR=0
#if defined(CERNLIB_VAX)
      IF(LIST(ISTART+16).EQ.0)CALL EPOPEN_VAX(2,IERR)
#endif
#if defined(CERNLIB_APOLLO)
      IF(LIST(ISTART+16).EQ.0) CALL EPOPEN_APOLLO(2)
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      IF(LIST(ISTART+16).EQ.0) CALL EPOPEN(2,IERR)
#endif
        if(IERR.ne.0)goto 20
      LIST(ISTART+16)=2
      IF(LIST(ISTART+17).LT.2)LIST(ISTART+17)=0
      MAXWDS=LIST(ISTART+1)
#if defined(CERNLIB_IBM)
      IF(LIST(ISTART+11).EQ.0.AND.LIST(ISTART+25).EQ.0)
     1  CALL IOOPTN(LUN,ULP,IRETCD)
#endif
#if defined(CERNLIB_NORD)
      NB=MAXWDS*2
      CALL IOREAD(LUN,IBUF,NB,IRETCD)
      IF(IRETCD.EQ.0)GOTO 50
      IF(IRETCD.EQ.1)GOTO 10
      IF(IRETCD.EQ.2)GOTO 20
      IF(IRETCD.EQ.3)GOTO 40
      IF(IRETCD.EQ.4)GOTO 30
      IF(IRETCD.EQ.8)GOTO 20
C--INVALID IOREAD ARGUMENT
 10   IERR=11
      GOTO 9999
#endif
#if defined(CERNLIB_IBM)
      NB=MAXWDS*2
      IF(LIST(ISTART+32).EQ.0)  THEN
         CALL IOREAD(LUN,IBUF,NB,IRETCD)
         IF(IRETCD.EQ.0)GOTO 50
         IF(IRETCD.EQ.1)GOTO 10
         IF(IRETCD.EQ.2)GOTO 20
         IF(IRETCD.EQ.3)GOTO 40
         IF(IRETCD.EQ.4)GOTO 30
         IF(IRETCD.EQ.8)GOTO 20
C--INVALID IOREAD ARGUMENT
 10      IERR=11
         GOTO 9999
      ELSE
*--- random access      HG  march 17, 89
         NRACR=LIST(ISTART+11)+1
         NRACW=MAXWDS/2
         READ(LUN,IOSTAT=IOS,REC=NRACR)  (IBUF(I),I=1,NRACW)
         IF(IOS.EQ.-1)GO TO 20
         NB=2*MAXWDS
         GOTO 50
      ENDIF
#endif
#if defined(CERNLIB_CDC)
      NB=(16*MAXWDS-1)/60+1
      BUFFER IN(LUN,1)(IBUF(1),IBUF(NB))
      IF(UNIT(LUN)) 5,20,40
    5 NB= LENGTH(LUN)*8
      GOTO 50
#endif
#if defined(CERNLIB_CRAY)
      IF(LIST(ISTART+32).EQ.0)  THEN
        NB=(16*MAXWDS-1)/LIST(4) + 1
        CALL READ (LUN,IBUF(1),NB,IRETCD,NUBC)
        GOTO (5,5,20,20,30,40), IRETCD+2
  5     NB=(LIST(4)*NB - NUBC)/8
        GOTO 50
      ELSE
*--- random access      MJC  Nov 23, 89
         NRACR=LIST(ISTART+11)+1
         NRACW=(16*MAXWDS-1)/LIST(4) + 1
         READ(LUN,IOSTAT=IOS,REC=NRACR)(IBUF(I),I=1,NRACW)
         IF(IOS.LT.0)GO TO 20
C
C   this can happen if Cray tries to read a whole no. of 4K blocks,
C   whereas actual data terminates with a partial block which has
C   not been padded (eg after transfer from VM).
C   If so, can be avoided by OMITTING  -s bin on the assign  for the
C   random access file to be read.
         IF(IOS.EQ.122)GO TO 30
         IF(IOS.GT.0)GO TO 40
         NB=2*MAXWDS
         GOTO 50
      ENDIF
C  --- end MJC random access
#endif
#if defined(CERNLIB_VAX)
       IF(LIST(ISTART+25).NE.0)THEN
C
C      SPECIAL READ FOR VARIABLE LENGTH RECORDS
C
       NB=0
       IOS=0
       CALL EPMTREAD(IBUF,MAXWDS*2,NB,IOS)
       IF(IOS.EQ.%LOC(SS$_NORMAL))THEN
          GO TO 50
       ELSE
        IF(IOS.EQ.%LOC(SS$_ENDOFFILE))GO TO 20
        IF(IOS.EQ.%LOC(SS$_ENDOFTAPE))GO TO 30
        IF(IOS.EQ.%LOC(SS$_PARITY))GO TO 40
        IF(IOS.EQ.%LOC(SS$_DATAOVERUN))GO TO 9902
        GO TO 9901
        ENDIF
       ELSE
C
C      THIS IS THE CASE OF DISK FILE
C
             IF(LIST(ISTART+32).EQ.0)  THEN
                READ(LUN,IOSTAT=IOS)(IBUF(I),I=1,MAXWDS)
                IF(IOS.EQ.-1)GO TO 20
                IF(IOS.EQ.FOR$IOS_INPRECTOO)GO TO 9902
                IF(IOS.EQ.FOR$IOS_ERRDURREA)GO TO 40
                NB=EPDKRECL(LUN)
             ELSE
*--- random access      HG  march 17, 89
                NRACR=LIST(ISTART+11)+1
                NRACW=LIST(ISTART+1)
                READ(LUN,IOSTAT=IOS,REC=NRACR)  (IBUF(I),I=1,NRACW)
                IF(IOS.EQ.-1)GO TO 20
                NB=2*LIST(ISTART+1)
             ENDIF
      ENDIF
             IF(NB.LE.0)GO TO 9901
             GO TO 50
#endif
#if defined(CERNLIB_APOLLO)
      IF(LIST(ISTART+25).NE.0)THEN
C
C      IN THE APOLLO CASE THE READING ROUTINE IS THE SAME FOR
C      MAGTAPES AND FOR DISK FILES
C
      IF(LIST(ISTART+32).EQ.0)   THEN
        NB=0
        CALL READ_$STREAM(IBUF,MAXWDS*2,NB,STATUS_$RETURNED)
        IF(STATUS_$RETURNED.EQ.STATUS_$OK) GO TO 50
        IF(LSHFT(ERROR_$SUBSYS(STATUS_$RETURNED),8)
     +  .NE.STREAM_$SUBS) THEN
          WRITE(NOUTUT,1000)
1000      FORMAT('  +++ EPIO/DOMAIN: Not a stream I/O error ???')
          GO TO 9901
        END IF
        ERROR_$RETURNED=ERROR_$CODE(STATUS_$RETURNED)
        IF(ERROR_$RETURNED.EQ.STREAM_$END_OF_FILE)          GO TO 20
        IF(ERROR_$RETURNED.EQ.STREAM_$BOF_ERR)              GO TO 30
        IF(ERROR_$RETURNED.EQ.STREAM_$INTERNAL_MM_ERROR.OR.
     *     ERROR_$RETURNED.EQ.STREAM_$INTERNAL_FATAL_ERROR) GO TO 40
        IF(ERROR_$RETURNED.EQ.STREAM_$INSUFF_MEMORY)GO TO 9902
C--- random access      M.Rumpf 01/91
       ELSE
            NRACR = LIST(ISTART+11) + 1
            NRACW =  MAXWDS
            READ(LUN,IOSTAT=IOS,REC=NRACR)(IBUF(I),I=1,NRACW)
            IF (IOS.EQ.-1) GO TO 20
            IF (IOS.GT.0)  GO TO 40
            IF(IOS.EQ.0) THEN
               NB = 2*MAXWDS
               GO TO 50
            ENDIF
       ENDIF
C --- end random access M.R
      END IF
C
C     HERE SIMPLY SOMETHING IS WRONG
C
      GO TO 9901
#endif
#if defined(CERNLIB_UNIVAC)
C--UNIT NUMBER IN FIELDATA CHARACTERS
      LUNFD=LIST(ISTART+24)
      NWDS=(MAXWDS*16+35)/36
C--FILE ADDRESS.  < 0 IF TAPE, >= 0 IF DISK
      IOAD=LIST(ISTART+25)
      IF(IOAD.GE.0) THEN
        NACC(1)=2**18+LOCF(NBLKSZ)
        NACC(2)=NWDS*2**18+LOCF(IBUF)
        CALL FIOSCR(LUNFD,NACC,2,IOAD,ISTAT)
       ELSE
        CALL TIOR(LUNFD,IBUF,NWDS,ISTAT,IAFH)
        NBLKSZ=IABS(ISTAT)
       ENDIF
      IF(ISTAT.EQ.-1.OR.ISTAT.EQ.-2) GOTO  20
      IF(IOAD.GE.0) THEN
C--IF DISK, NBLKSZ MUST CONTAIN ('EP',BLOCK SIZE) FOR A NORMAL BLOCK,
C--  OR 'EOFM' FOR END-OF-FILE BLOCK.
        IF(NBLKSZ.EQ.IEOF) GOTO 20
        IEPR=BITS(NBLKSZ,1,18)
        IF(IEPR.EQ.IEP) THEN
          NBLKSZ=BITS(NBLKSZ,19,18)
         ELSE
C--DISK FILE IS NOT IN EP-FORMAT -> ERROR 19 (UNIVAC ONLY)
          IERR=19
          GOTO 9999
         ENDIF
       ENDIF
      IF(ISTAT.LE.-6) THEN
C--IERR=2+100*IABS(UNIVAC ERROR CODE)
        IERR=2+IABS(ISTAT)*100
        LIST(ISTART+13)=LIST(ISTART+13)+1
        GOTO 9999
       ENDIF
      NB=(36*NBLKSZ)/8
      GOTO 50
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      MAXWDS=MAXWDS/N16PW
      IF(LIST(ISTART+33).EQ.2)THEN
      NWDONE=MAXWDS
        IF(LIST(ISTART+32).EQ.1)  THEN
          call cfseek(list(istart+25),0,MAXWDS,LIST(ISTART+11),ISTAT)
          IF(ISTAT.NE.0)goto 45
        ENDIF
        call cfget(list(istart+25),0,MAXWDS,NWDONE,IBUF,ISTAT)
        if(ISTAT.eq.-1)goto 20
        if(ISTAT.ne.0 )goto 40
      ELSE IF(LIST(ISTART+33).EQ.1)THEN
C In random mode hitting EOF is return as an error.
C As disk errors are rare, assume they are End of File
#endif
#if (defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX))&&(!defined(CERNLIB_STF77IB))
       READ(LUN,ERR=20,IOSTAT=IOSEOF,REC=LIST(ISTART+11)+1)
     1       (IBUF(IWRD),IWRD=1,MAXWDS)
       IF (IOSEOF.EQ.-1) GOTO 20
#endif
#if defined(CERNLIB_STF77IB)
       READ(LUN,NUM=NB,ERR=20,REC=LIST(ISTART+11)+1)
     1      (IBUF(IWRD),IWRD=1,MAXWDS)
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      ELSE
#endif
#if (defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX))&&(!defined(CERNLIB_STF77IB))
       READ(LUN,ERR=40,END=20)(IBUF(IWRD),IWRD=1,MAXWDS)
#endif
#if defined(CERNLIB_STF77IB)
      READ(LUN,NUM=NB,ERR=40,END=20)(IBUF(IWRD),IWRD=1,MAXWDS)
#endif
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      ENDIF
      NB=MAXWDS*N16PW*2
      GOTO 50
#endif
C--EOF or OPEN ERROR, CANNOT FIND FILE
 20   IF(LIST(ISTART+19) .EQ. 1)GOTO 30
      LIST(ISTART+19)=1
      IERR=1
      GOTO 9999
C EOI
 30   IERR=3
      GOTO 9999
C PARITY
 40   LIST(ISTART+13)=LIST(ISTART+13)+1
      IERR=2
#if defined(CERNLIB_CRAY)
      IF(LIST(ISTART+32).EQ.0) THEN
        CALL SKIPBAD(LUN,NBLOCK,IRETCD)
        PRINT *,' HARWARE ERROR ON READ ',NBLOCK,' BLOCKS SKIPPED'
        IF(IRETCD.EQ.0) PRINT *,
     &  ' NONZERO RETURN CODE FROM SKIPBAD, SERIOUS PROBLEM?'
      ENDIF
#endif
      GOTO 9999
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
C Error from cfseek
 45   IERR=25
      GOTO 9999
#endif
C GOOD BLOCK
 50   LIST(ISTART+11)=LIST(ISTART+11) +1
CD    LABEL=1050
CD    IF(DBUGFL)PRINT*,LABEL,ISTART,(LIST(ISTART+JJJ),JJJ=11,22)
C RESET EOF FLAG
      LIST(ISTART+19)=0
#if defined(CERNLIB_UNIVAC)
C--UPDATE FILE ADDRESS IF DISK. WE ASSUME F-FORMAT WITH 28 WORDS/SECTOR
      IF(IOAD.GE.0) LIST(ISTART+25)=LIST(ISTART+25)+(NBLKSZ+28)/28
#endif
      IF(LIST(ISTART+18) .EQ. 0)GOTO 70
C--HEADERLESS BLOCK
C--- IF CONTROL WORD 27 SET, SWAP BYTES IF DONE IN PREVIOUS BLOCK
      IF(LIST(ISTART+28).NE.0)  CALL BTSWAP(IBUF,NB)
      LIST(ISTART+7)=0
      LIST(ISTART+15)=0
      LIST(ISTART+18)=LIST(ISTART+18) - 1
      LIST(ISTART+14)=NB/2
      GOTO 77777
C--BLOCK WITH HEADER
 70   CONTINUE
*--- could be 32 bit header - blow 16 16-bit words to check
      CALL BLO16W(IBUF,1,ILBHD,1,16)
*--- make sure it is not 16-bit
         LIST(ISTART+28)=0
         LIST(ISTART+29)=0
         IF(LIST(ISTART+27).EQ.0)  GOTO 72
         IF(ILBHD(7).EQ.29954.AND.ILBHD(8).EQ.31280)  GOTO 72
*---    or byte swapped 16-bit
         IF(ILBHD(7).EQ.  629.AND.ILBHD(8).EQ.12410)  GOTO 71
*--- check for 32 bit - control words 7 and 8 (identical)
*     first number is 7967 = 1F1F hex and therefore invariant under
*     byte swapping, second is 19132 = 4ABC hex and is used to byte swap
      IF(ILBHD(13).EQ.7967.AND.ILBHD(15).EQ.7967)  THEN
*---  32 bit header - byte swap ?
         IF(ILBHD(14).NE.19132)  THEN
            CALL BTSWAP(IBUF,NB)
            CALL BLO16W(IBUF,1,ILBHD,1,16)
            LIST(ISTART+28)=1
         ENDIF
         IF(ILBHD(14).NE.19132.OR.ILBHD(16).NE.19132)  THEN
*--- looks like 32 bit, but is not
            IERR=22
            GOTO 9999
         ENDIF
*--- definitly 32 bit physical header
         CALL BLO32W(IBUF,1,ILBHD,1,12)
         CALL CFRIBM(ILBHD,12,2)
*--- set 32 bit ph. header
         LIST(ISTART+29)=1
         NP=ILBHD(1)
         IF(NP.LE.0) GOTO 9901
         IF(NP .GT. LIST(ISTART+1).OR.NP.GT.NB/2) GOTO 9902
         NPHL=ILBHD(2)
         IF(NPHL.LT.LIST(6).OR.NPHL.GT.NP)  THEN
            IERR=7
            GOTO 9999
         ENDIF
      ENDIF
      IF(LIST(ISTART+29).EQ.1)GOTO 150
*--- 16 bit physical header
C--- SWAP BYTES IF CONTROL WORD 27 SET, AND IF 16-BIT WORDS 7 AND 8
C    IN THE BLOCK HEADER ARE WRONG FOR EP FORMAT.
   71    CONTINUE
         CALL BTSWAP(IBUF,NB)
         CALL BLO16W(IBUF,1,ILBHD,1,12)
         LIST(ISTART+28)=1
   72    CONTINUE
         NP=ILBHD(1)
CD    LABEL=1070
CD    IF(DBUGFL)PRINT*,LABEL,ISTART,(LIST(ISTART+JJJ),JJJ=11,22)
         IF(NP.LE.0) GOTO 9901
         IF(NP .GT. LIST(ISTART+1).OR.NP.GT.NB/2) GOTO 9902
         NPHL=ILBHD(2)
         IF (NPHL. LT. LIST(6) .OR. NPHL .GT. NP
     1   .OR. (ILBHD(7).NE.29954) .OR. (ILBHD(8).NE.31280))
     2   GO TO 110
*--- set 16 bit header
         LIST(ISTART+29)=0
         GO TO 150
 110     CONTINUE
         IF(LIST(ISTART+27).LT.2)GOTO 120
         IERR=7
         GOTO 9999
C--OLD FORMAT
 120     IF (NPHL .LT.5 .OR. NPHL.GT.NP)IERR=7
         IF (IERR .NE.0) GOTO 9999
         LIST(ISTART+17) =1
         LIST(ISTART+3)=16
C--WE HAVE A GOOD HEADER - 16 bit or 32 bit (ILBHD(6) = 0 or 1)
  150 CONTINUE
      IF(LIST(ISTART+17).EQ.1)  GOTO 160
      LIST(ISTART+3)=ILBHD(11)
      LIST(ISTART+9)=ILBHD(6)
      LIST(ISTART+18)=ILBHD(9)
  160 CONTINUE
      LIST(ISTART+14)=NP
      LIST(ISTART+15)=ILBHD(4)
      LIST(ISTART+7)=NPHL
      LIST(ISTART+4)=ILBHD(5)
77777 RETURN
 9901 CONTINUE
C--- BLOCK LENGTH (IBUF(1)) LE 0 - PROBABLY NOT EP FORMAT
      IERR=4
      GOTO 9999
 9902 CONTINUE
C--- IBUF(1) GT REAL PRL OR UNIT BUFFER TOO SMALL
      IERR=5
 9999 CONTINUE
      LIST(ISTART+14)=0
      GOTO 77777
      END
