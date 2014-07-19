*
* $Id: czdopn.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czdopn.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDOPN( CHSERV, HOST, ISTAT )
*
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
      REAL DBUF
      COMMON /CDBUF/ DBUF(10)
*
      LOGICAL STATUS
*
      CHARACTER*20 CHPROX
      CHARACTER*(*) HOST, CHSERV
      CHARACTER*80 TASK
      CHARACTER*22 CHFILE
      CHARACTER*20 SERVICE
      CHARACTER*12 USER,CHMODE
      CHARACTER*64 PASS
*
      INTEGER ISTAT, LIB$GET_TERMINAL, LIB$GET_NOECHO, LIB$GET_LUN
     +,       LENU, LENP, JUMPAD, LENOCC, LIB$GETJPI
 
      INCLUDE '($JPIDEF)'
*
      EXTERNAL CZTCP
*
      LH = LENOCC(HOST)
      IF(IDEBXZ.GE.0) PRINT *,'CZDOPN. Using DECnet to access ',
     +   HOST(1:LH)
      ISTAT = 0
*
*-- Get logical unit for network connection
*
      STATUS = LIB$GET_LUN(LUNDEC)
      IF(.NOT.STATUS) GO TO 99
*
*   Check logical name CZPROXY. If set to TRUE, do not prompt
*   for username and password
*
      STATUS = LIB$SYS_TRNLOG('CZPROXY',LPROXY,CHPROX,,,)
      CALL CLTOU(CHPROX)
      IF(CHPROX(1:LPROXY).NE.'TRUE') THEN
*
*     Check if we are interactive...
*
         STATUS = LIB$GETJPI(JPI$_MODE,,,,CHMODE,%REF(LMODE))
*
*-- Get user name and password
*
         IF(CHMODE(1:LMODE).EQ.'INTERACTIVE') THEN
            STATUS = LIB$GET_TERMINAL(USER,'Username? ', LENU)
            IF(.NOT.STATUS) GO TO 99
         ELSE
            LENU = 0
         ENDIF
         IF(LENU.NE.0) THEN
            STATUS = LIB$GET_NOECHO(PASS,'Password? ', LENP)
            IF(.NOT.STATUS) GO TO 99
         ENDIF
      ELSE
         LENU = 0
      ENDIF
*
*-- Request network connection
*
      IF(LENU.NE.0) THEN
         TASK = HOST(1:LH)//'"'//USER(1:LENU)//' '//PASS(1:LENP)
     +                   //'"::"TASK='//CHSERV//'"'
      ELSE
         TASK = HOST(1:LH)//'::"TASK='//CHSERV//'"'
      ENDIF
      OPEN(UNIT=LUNDEC,FILE=TASK(1:LENOCC(TASK))
     +,     STATUS='OLD',ERR=99)
      PRINT *,'Connection to ',HOST(1:LH),' established'
      ISKIN = LUNDEC
      ISKOUT= LUNDEC
*
*-- Set up FZ in channel mode
*
      IF( IADTCP .EQ. 0 )THEN
         LUNCZ= 6
         LBUF = 270
         CALL FZFILE(LUNFZI, LBUF, 'SIOC')
         CALL FZHOOK(LUNFZI, CZTCP, DBUF)
         CALL FZFILE(LUNFZO, LBUF, 'SOC')
         CALL FZHOOK(LUNFZO, CZTCP, DBUF)
         IADTCP = JUMPAD(CZTCP)
         CALL JUMPST(IADTCP)
      ENDIF
      RETURN
   99 CONTINUE
      ISTAT = 1
      END
