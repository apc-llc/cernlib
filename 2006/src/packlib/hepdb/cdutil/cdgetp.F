*
* $Id: cdgetp.F,v 1.1.1.1 1996/02/28 16:24:45 mclareni Exp $
*
* $Log: cdgetp.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:45  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB_UNIX)
      SUBROUTINE CDGETP(LUN,IPID,IRC)
 
      CHARACTER*80 CHSERV,CHLINE,CHLOG
      CHARACTER*8  CHHOST,CHNODE
 
#include "hepdb/slate.inc"
 
      IPID = 0
      IRC  = 0
 
      CALL GETENVF('CDSERV',CHSERV)
 
      IF(IS(1).EQ.0) THEN
         IRC = -3
         GOTO 99
      ENDIF
*
*     Names file is $CDSERV/hepdb.names
*
      OPEN(LUN,FILE=CHSERV(1:IS(1))//'/hepdb.names',
     +     STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +     IOSTAT=IRC)
      IF(IRC.NE.0) GOTO 99
*
*     Read all records until :log. found
*
10    CONTINUE
      READ(LUN,'(A)',END=98,ERR=98) CHLINE
      LLINE = LENOCC(CHLINE)
 
      LLOG  = INDEX(CHLINE(1:LLINE),':log.')
      IF(LLOG.EQ.0) GOTO 10
 
      CLOSE(LUN)
      CHLOG = CHLINE(LLOG+5:LLINE)
      LLOG  = LLINE - 4
      LBLA  = INDEX(CHLOG(1:LLOG),' ')
      IF(LBLA.NE.0) LLOG = LBLA -1
 
      OPEN(LUN,FILE=CHLOG(1:LLOG)//'/hepdb.log',
     +     STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +     IOSTAT=IRC)
      IF(IRC.NE.0) GOTO 99
 
20    CONTINUE
      READ(LUN,'(A)',END=98,ERR=98) CHLINE
      LLINE = LENOCC(CHLINE)
 
      LPID = INDEX(CHLINE(1:LLINE),'CDSERV. PID = ')
      IF(LPID.EQ.0) GOTO 20
 
      LNODE = INDEX(CHLINE(1:LLINE),' HOST = ')
      IF(LNODE.EQ.0) GOTO 20
 
      CHNODE = CHLINE(LNODE+8:LNODE+16)
      LBLA   = INDEX(CHNODE,' ')
      IF(LBLA.NE.0) THEN
         LNODE = LBLA - 1
      ELSE
         LNODE = 8
      ENDIF
 
      CALL CDHOST(CHHOST,IRC)
      IF(IRC.NE.0) GOTO 98
 
      LHOST = LENOCC(CHHOST)
 
      IF(CHHOST(1:LHOST).NE.CHNODE(1:LNODE)) GOTO 98
 
      IPID = ICDECI(CHLINE,LPID+14,LLINE)
 
98    CLOSE(LUN)
 
99    END
#endif
