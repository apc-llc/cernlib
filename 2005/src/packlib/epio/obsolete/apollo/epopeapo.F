*
* $Id: epopeapo.F,v 1.1.1.1 1996/03/08 15:21:50 mclareni Exp $
*
* $Log: epopeapo.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:50  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_APOLLO)
      SUBROUTINE EPOPEN_APOLLO(MODE)
%INCLUDE '/sys/ins/base.ins.ftn'
%INCLUDE '/sys/ins/error.ins.ftn'
%INCLUDE '/sys/ins/streams.ins.ftn'
#include "epio/epiocom.inc"
#include "epio/epapocom.inc"
*
*         DOES FILE OPEN FOR APOLLO, VARIABLE RECORD LENGTH FILES
*
*  INPUT:
*
*  MODE     =1 FOR WRITE, =2 FOR READ
*
      CHARACTER*256 FLNAM
      INTEGER*2 STREAM_$ID,ATT_$REC(1)
      INTEGER*4 STATUS_$RETURNED,ATT_$REC_4(16),ERROR_$MASK
      EQUIVALENCE (ATT_$REC,ATT_$REC_4)
      LOGICAL OPEND
*
      LUN        = LIST(ISTART+10)
      LREC       = LIST(ISTART+1)*2
      STREAM_$ID = 0
      CALL VEC_$IINIT(ATT_$REC_4,16,0)
*
*           FIRST OF ALL WE CHECK IF THE UNIT HAS BEEN OPEN BY
*                     A FORTRAN OPEN STATEMENT
*
      INQUIRE(UNIT=LUN,STRID=STREAM_$ID,NAME=FLNAM,OPENED=OPEND)
      IF(OPEND) THEN
*
*           THE UNIT IS ALREADY OPEN. WE JUST REGISTER THE NAME
*
        EPVXUN(LREF)    = FLNAM
      ELSE
        STREAM_$ID = 0
        IF(LIST(ISTART+25).NE.0) THEN
*
*            MAYBE THE STREAM HAS BEEN OPENED AND CLOSED BUT THE
*                     STREAM_$ID NOT ZEROED
*
          ATT_$REC(1)     = INT2(LIST(ISTART+25))
          CALL STREAM_$INQUIRE(
     1          STREAM_$IRM_OTYPE,
     2           STREAM_$USE_STRID,
     3            ATT_$REC,
     4             ERROR_$MASK,
     5              STATUS_$RETURNED)
          IF  (ERROR_$CODE(STATUS_$RETURNED).NE.STREAM_$NOT_OPEN.AND.
     +    ERROR_$CODE(STATUS_$RETURNED).NE.STREAM_$STREAM_NOT_FOUND)
     +    THEN
            IF(STATUS_$RETURNED.EQ.STATUS_$OK) THEN
*
*         HERE THE STREAM WAS OPEN BUT NOT BY A FORTRAN OPEN:
*              NOT ALLOWED, WE JUST CLOSE THE STREAM
*
              CALL STREAM_$CLOSE(ATT_$REC,STATUS_$RETURNED)
              IF(STATUS_$RETURNED.NE.STATUS_$OK)
     +        CALL ERROR_$PRINT(STATUS_$RETURNED)
            ELSE
*
*          HERE SOMETHING ELSE IS WRONG
*
              CALL ERROR_$PRINT(STATUS_$RETURNED)
            END IF
          END IF
        END IF
*
*            USE USER SPECIFIED NAME, OR DEFAULT NAME 'FOR0NN.DAT',
*                  ALSO FOR THE MAGTAPE DESCRIPTOR FILE
*
        IF (EPVXUN(LREF)(1:1).EQ.' ')  THEN
          WRITE(FLNAM,1000)LUN/10,MOD(LUN,10)
 1000     FORMAT('FOR0',2I1,'.DAT')
          EPVXUN(LREF)=FLNAM
          NSLNG = 10
        ELSE
          FLNAM = EPVXUN(LREF)
          DO NSLNG=256,1,-1
            IF(EPVXUN(LREF)(NSLNG:NSLNG).NE.' ') GO TO 82
          END DO
  82      CONTINUE
        END IF
      ENDIF
      LIST(ISTART+24) = 0
*
      IF(MODE.NE.1)  THEN
*
*                HERE WE OPEN FOR READING
*
        IF(STREAM_$ID.EQ.0) THEN
          CALL STREAM_$OPEN (
     1     FLNAM,
     2      INT2(NSLNG),
     3       STREAM_$READ,
     4        STREAM_$NO_CONC_WRITE,
     5         STREAM_$ID,
     6          STATUS_$RETURNED )
          IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1    CALL ERROR_$PRINT(STATUS_$RETURNED)
        END IF
C
C                SEE WHETHER THIS IS A TAPE UNIT
C
        ATT_$REC(1)=STREAM_$ID
        CALL STREAM_$INQUIRE (
     1    STREAM_$IRM_OTYPE,
     2     STREAM_$USE_STRID,
     3      ATT_$REC,
     4       ERROR_$MASK,
     5        STATUS_$RETURNED)
        IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1  CALL ERROR_$PRINT(STATUS_$RETURNED)
        IF(ATT_$REC_4(14).NE.16#314) CONTINUE
C
C     NOW REDEFINE MOVE MODE
C
        ATT_$REC(5)=16#1000
        CALL STREAM_$REDEFINE(
     1   STREAM_$ID,
     2    STREAM_$IRM_EXPLICIT_ML,
     3     ATT_$REC,
     4      ERROR_$MASK,
     5       STATUS_$RETURNED)
        IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1  CALL ERROR_$PRINT(STATUS_$RETURNED)
      ELSE
*
*                HERE WE OPEN FOR WRITING
*
        IF(STREAM_$ID.EQ.0) THEN
          CALL STREAM_$OPEN (
     1     FLNAM,
     2      INT2(NSLNG),
     3       STREAM_$WRITE,
     4        STREAM_$NO_CONC_WRITE,
     5         STREAM_$ID,
     6          STATUS_$RETURNED )
          IF (STATUS_$RETURNED .NE. STATUS_$OK.AND.
     1    ERROR_$CODE(STATUS_$RETURNED).NE.STREAM_$NAME_NOT_FOUND)
     2    CALL ERROR_$PRINT(STATUS_$RETURNED)
        END IF
        IF(STREAM_$ID.NE.0) THEN
*
*                SEE WHETHER THIS IS A TAPE UNIT
*
          ATT_$REC(1)=STREAM_$ID
          CALL STREAM_$INQUIRE (
     1     STREAM_$IRM_OTYPE,
     2      STREAM_$USE_STRID,
     3       ATT_$REC,
     4        ERROR_$MASK,
     5         STATUS_$RETURNED)
          IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1    CALL ERROR_$PRINT(STATUS_$RETURNED)
          IF(ATT_$REC_4(14).EQ.16#314) GO TO 100
        ELSE
*
*                HERE THE STREAM NEEDS TO BE CREATED
*
          CALL STREAM_$CREATE_BIN(
     1      FLNAM,
     2       INT2(NSLNG),
     3        STREAM_$APPEND,
     4         STREAM_$NO_CONC_WRITE,
     5          STREAM_$ID,
     6           STATUS_$RETURNED)
          IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1    CALL ERROR_$PRINT(STATUS_$RETURNED)
        END IF
*
*                 THIS IS A DISK FILE
*
*                 ASK FOR APPEND ACCESS
*
        ATT_$REC(6)=16#200
        CALL STREAM_$REDEFINE(
     1   STREAM_$ID,
     2    STREAM_$IRM_OPOS,
     3     ATT_$REC,
     4      ERROR_$MASK,
     5       STATUS_$RETURNED)
        IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1  CALL ERROR_$PRINT(STATUS_$RETURNED)
C
C     NOW REDEFINE NON BINARY
C
        ATT_$REC(5)=0
        CALL STREAM_$REDEFINE(
     1   STREAM_$ID,
     2    STREAM_$IRM_AB_FLAG,
     3     ATT_$REC,
     4      ERROR_$MASK,
     5       STATUS_$RETURNED)
        IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1  CALL ERROR_$PRINT(STATUS_$RETURNED)
C
C     NOW REDEFINE IT AS VARIABLE RECORD LENGTH
C
        ATT_$REC(5)=16#0
        CALL STREAM_$REDEFINE(
     1   STREAM_$ID,
     2    STREAM_$IRM_EXPLICIT_TYPE,
     3     ATT_$REC,
     4      ERROR_$MASK,
     5       STATUS_$RETURNED)
        IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1  CALL ERROR_$PRINT(STATUS_$RETURNED)
C
C     WE WANT TO REDEFINE IT AS MOVE MODE
C
        ATT_$REC(5)=16#1000
        CALL STREAM_$REDEFINE(
     1   STREAM_$ID,
     2    STREAM_$IRM_EXPLICIT_ML,
     3     ATT_$REC,
     4      ERROR_$MASK,
     5       STATUS_$RETURNED)
        IF (STATUS_$RETURNED .NE. STATUS_$OK)
     1  CALL ERROR_$PRINT(STATUS_$RETURNED)
      ENDIF
 100  CONTINUE
C
C      NOW STORE THE STREAM-ID
C
      LIST(ISTART+25) = INT(STREAM_$ID)
      RETURN
      END
#endif
