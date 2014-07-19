*
* $Id: fafile.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fafile.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAFILE(LUN,FILE,IRC)
*CMZ :          21/08/91  15.13.43  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/08/91
#if defined(CERNLIB_IBMMVS)
      DIMENSION     DISP(3)
      DIMENSION     SPACE(4)
#include "fatmen/fattyp.inc"
#endif
      CHARACTER*(*) FILE
      CHARACTER*80  OUTPUT
 
      CHARACTER*3   CHSTAT
 
      LOGICAL       IEXIST
 
      LOUT   = LENOCC(FILE)
      OUTPUT = FILE
 
      IRC = 0
 
#if defined(CERNLIB_UNIX)
      CALL CUTOL(OUTPUT(1:LOUT))
#endif
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',OUTPUT,1,LOUT)
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
      INQUIRE(FILE='/'//OUTPUT(1:LOUT),EXIST=IEXIST)
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMMVS))
      INQUIRE(FILE=OUTPUT(1:LOUT),EXIST=IEXIST)
#endif
      IF(IEXIST) THEN
         CHSTAT = 'OLD'
      ELSE
         CHSTAT = 'NEW'
      ENDIF
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',OUTPUT,1,LOUT)
      CALL FILEINF(ISTAT,'LRECL',133,'RECFM','V')
      OPEN(LUN,FILE='/'//OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
     +     STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
#endif
#if defined(CERNLIB_IBMMVS)
      LUNIT = LENOCC(CHMGEN(1))
      CALL FILEINF(ISTAT,'DEVICE',CHMGEN(1)(1:LUNIT),'TRK',1, 'SECOND',
     +1,'DIR',0, 'RECFM','VBA','LRECL',137,'BLKSIZE',6233)
      OPEN(LUN,FILE='/'//OUTPUT(1:LOUT),ACCESS='SEQUENTIAL', ACTION=
     +'READWRITE',
     +     STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IEXIST) THEN
         OPEN(LUN,FILE=OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
     +        STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
      ELSE
         OPEN(LUN,FILE=OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
     +        CARRIAGECONTROL='LIST',
     +        STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      OPEN(LUN,FILE=OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
     +      STATUS=CHSTAT,FORM='FORMATTED',IOSTAT=IRC)
#endif
 
      END
