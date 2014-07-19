*
* $Id: epopestf.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: epopestf.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_STF77)||defined(CERNLIB_STF77VX)
      SUBROUTINE EPOPEN(MODE,IERR)
*
*         STANDARD FORTRAN FILE OPEN
*
*  INPUT:
*
*  MODE     =1 FOR WRITE, =2 FOR READ
*
#include "epio/epiocom.inc"
#if defined(CERNLIB_STF77)
#include "epio/epapocom.inc"
      CHARACTER*256 FLNAM
      PARAMETER (LFLNAM=256)
#endif
#if defined(CERNLIB_STF77VX)
#include "epio/epvaxcom.inc"
      CHARACTER*255 FLNAM
      PARAMETER (LFLNAM=255)
#endif
#if defined(CERNLIB_STF77IB)
C     STR151 is used by ERRSAV to eliminate message AFB151
      CHARACTER*8 STR151
#endif
      LOGICAL OPEND
*
      LUN        = LIST(ISTART+10)
      LREC       = LIST(ISTART+1)*2
*
*           FIRST OF ALL WE CHECK IF THE UNIT HAS BEEN OPEN BY
*                     A FORTRAN OPEN STATEMENT
*
       IF(LIST(ISTART+33).LT.2)THEN
      INQUIRE(UNIT=LUN,NAME=FLNAM,OPENED=OPEND,IOSTAT=IOS)
        IF(IOS.NE.0)GOTO 9901
      IF(OPEND) THEN
*
*           THE UNIT IS ALREADY OPEN. WE JUST REGISTER THE NAME
*
        EPVXUN(LREF)    = FLNAM
        GOTO 77777
      END IF
       END IF
*
*            USE USER SPECIFIED NAME, OR DEFAULT NAME 'for0nn',
*
        IF (EPVXUN(LREF)(1:1).EQ.' ')  THEN
#if !defined(CERNLIB_STF77IB)
*
*  On Unix, use for0nn for Fortran sequential and
*               epionn for Fortran direct access and C I/O names
*
            IF(LIST(ISTART+33).LT.1)THEN
          WRITE(FLNAM,1000)LUN/10,MOD(LUN,10)
 1000     FORMAT('for0',2I1)
          NSLNG = 6
            ELSE
          WRITE(FLNAM,1100)LUN/10,MOD(LUN,10)
 1100     FORMAT('epio',2I1)
          NSLNG = 6
            ENDIF
#endif
#if defined(CERNLIB_STF77IB)
 1000     FORMAT('/FOR0',2I1,' DAT ')
          NSLNG = 12
*
#endif
          EPVXUN(LREF)=FLNAM
        ELSE
          FLNAM = EPVXUN(LREF)
          DO 10 NSLNG=LFLNAM,1,-1
            IF(EPVXUN(LREF)(NSLNG:NSLNG).NE.' ') GO TO 82
  10      CONTINUE
  82      CONTINUE
        END IF
C?    LIST(ISTART+24) = 0
*
      IF(MODE.EQ.2)  THEN
*
*                HERE WE OPEN FOR READING
*
        IF(LIST(ISTART+33).EQ.1)THEN
#if defined(CERNLIB_STF77IB)
         CALL FILEINF(IRC,'MAXREC',16777215)
#endif
         OPEN(LUN,FILE=EPVXUN(LREF)(1:NSLNG),STATUS='OLD',
#if defined(CERNLIB_STF77VX)
     1       ACCESS='DIRECT',RECL=LIST(ISTART+1)/2,
#endif
#if defined(CERNLIB_STF77IB)||defined(CERNLIB_STF77)
     1       ACCESS='DIRECT',RECL=LIST(ISTART+1)*2,
#endif
     1       FORM='UNFORMATTED',IOSTAT=IOS)
        ELSE IF(LIST(ISTART+33).EQ.0)THEN
         OPEN(LUN,FILE=EPVXUN(LREF)(1:NSLNG),STATUS='OLD',
     1       FORM='UNFORMATTED',IOSTAT=IOS)
        ELSE IF(LIST(ISTART+33).EQ.2)THEN
         call cfopen(LIST(ISTART+25),0,LIST(ISTART+1)/2,'r',
     1       0,EPVXUN(LREF)(1:NSLNG),IOS)
        ENDIF
        IF(IOS.NE.0)GOTO 9901
      ELSE
*
*                HERE WE OPEN FOR WRITING
        IF(LIST(ISTART+33).EQ.1)THEN
#if defined(CERNLIB_STF77IB)
      CALL ERRSET(151,256,-1,1,0,0)
      CALL ERRSAV(151,STR151)
      CALL FILEINF(IRC,'MAXREC',2)
      OPEN (UNIT=LUN,STATUS='UNKNOWN',ACCESS='DIRECT'
     1     ,FORM='UNFORMATTED',RECL=LIST(ISTART+1)*2
     2     ,FILE=EPVXUN(LREF)(1:NSLNG))
      CLOSE(LUN)
      CALL ERRSTR(151,STR151)
      CALL FILEINF(IRC,'MAXREC',16777215)
#endif
         OPEN(LUN,FILE=EPVXUN(LREF)(1:NSLNG),STATUS='UNKNOWN',
#if defined(CERNLIB_STF77VX)
     1       ACCESS='DIRECT',RECL=LIST(ISTART+1)/2,
#endif
#if defined(CERNLIB_STF77IB)||defined(CERNLIB_STF77)
     1       ACCESS='DIRECT',RECL=LIST(ISTART+1)*2,
#endif
     1       FORM='UNFORMATTED',IOSTAT=IOS)
        ELSE IF(LIST(ISTART+33).EQ.0)THEN
*
         OPEN(LUN,FILE=EPVXUN(LREF)(1:NSLNG),STATUS='NEW',
     1       FORM='UNFORMATTED',IOSTAT=IOS)
        ELSE IF(LIST(ISTART+33).EQ.2)THEN
         call cfopen(LIST(ISTART+25),0,LIST(ISTART+1)/2,'w+',
     1       0,EPVXUN(LREF)(1:NSLNG),IOS)
        ENDIF
        IF(IOS.NE.0)GOTO 9901
      ENDIF
      GOTO 77777
C       ERROR TRYING TO OPEN
 9901 CONTINUE
        IERR=1
77777 RETURN
      END
#endif
