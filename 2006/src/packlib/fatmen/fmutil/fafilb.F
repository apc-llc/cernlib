*
* $Id: fafilb.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fafilb.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAFILB(LUN,FILE,LRECL,IRC)
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
            CHSTAT  = 'OLD'
         ELSE
            CHSTAT  = 'NEW'
         ENDIF
#if defined(CERNLIB_IBMVM)
         CALL FILEINF(ISTAT,'RECFM','F','LRECL',LRECL)
         CALL CTRANS('.',' ',OUTPUT,1,LOUT)
         OPEN(LUN,FILE='/'//OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
#endif
#if defined(CERNLIB_IBMMVS)
         LUNIT = LENOCC(CHMGEN(1))
         CALL FILEINF(ISTAT,'DEVICE',CHMGEN(1)(1:LUNIT),'TRK',1,
     +                'SECOND',1,'DIR',0,
     +                'RECFM','F','LRECL',LRECL)
         OPEN(LUN,FILE='/'//OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
     +      ACTION='READWRITE',
#endif
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_IBMMVS))
         OPEN(LUN,FILE=OUTPUT(1:LOUT),ACCESS='SEQUENTIAL',
#endif
     +      STATUS=CHSTAT,FORM='UNFORMATTED',IOSTAT=IRC)
 
       END
