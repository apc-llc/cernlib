*
* $Id: zs.F,v 1.5 2005/04/18 15:23:09 mclareni Exp $
*
* $Log: zs.F,v $
* Revision 1.5  2005/04/18 15:23:09  mclareni
* Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
*
* Revision 1.4  1997/10/21 17:11:54  mclareni
* Remove print statements and NT tabs
*
* Revision 1.3  1997/09/19 14:09:36  mclareni
* Remove NT editor eols
*
* Revision 1.2  1997/09/02 08:46:16  mclareni
* WINNT mods, mostly cpp defines
*
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      INDIRECT PAWC,HOS9C2
#endif
      PROGRAM ZS
********************************************************************
*                                                                  *
*          Remote server for PAW.                                  *
*          Current implementation based on TCP/IP.                 *
*          The server is automatically started when the            *
*          PAW command RLOGIN is invoked.                          *
*          When the PAW command RSHELL is invoked, the             *
*          string following the command is passed to the server.   *
*          Transfer of ZEBRA structures is automatically handled   *
*          from the internal routines of PAW/HBOOK.                *
*                                                                  *
*          Note that the small communication packages CZ (Fortran) *
*          and TCPAW (C) must be linked with ZS.                   *
*                                                                  *
*            Author: Rene Brun CERN/DD                             *
*                  in collaboration with :                         *
*                             P.Scharff-Hansen for OS9 (OPAL)      *
*                             J.Raab (OPAL)                        *
*                             B.Segal (DD) for TCPAW               *
*                                                                  *
********************************************************************
#if defined(CERNLIB_OS9)
#include "cspack/hos9c.inc"
#endif
#include "cspack/czhome.inc"
#include "cspack/slate.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
#if defined(CERNLIB_DECNET)
#include "cspack/czdecnet.inc"
#endif
#include "cspack/czdir.inc"
#include "cspack/zunit.inc"
#include "cspack/pawc.inc"
      COMMON/CDBUF/DBUF(10)
      DIMENSION IHMAIL(20)
#if !defined(CERNLIB_IBM)||defined(CERNLIB_TCPSOCK)
      INTEGER STATUS
      INTEGER SSETUP,SCLOSE
      CHARACTER*26 CHFILE
#endif
#if defined(CERNLIB_IBM)
      CHARACTER*80 ARGS
*     CHARACTER*10 PORT
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*12 REMNOD
      LOGICAL     ILOGIN
      INTEGER     SYSTEMF
#endif
      CHARACTER*80 CHMAIL
      CHARACTER*6 CHMESS
      EXTERNAL CZTCP
      EXTERNAL ZSRZIN
 
      PARAMETER (IPRINT=77)
      PARAMETER (IDEBUG=0)
      PARAMETER (LUNI=78)
      PARAMETER (LUNO=79)
*
*     Start server
*
#if defined(CERNLIB_UNIX) && !defined(CERNLIB_WINNT)
      ISOCK=ISETUP(ISKIN,ISKOUT)
      LUNCZ=77
      CHFILE=' '
      CALL DATIME(ND,NT)
      WRITE(CHFILE,1000)ND,NT
 1000 FORMAT('/var/log/zs',I6,I4,'.log')
      DO 1 I=11,23
         IF(CHFILE(I:I).EQ.' ')CHFILE(I:I)='0'
    1 CONTINUE
      OPEN(UNIT=LUNCZ,FILE=CHFILE,STATUS='UNKNOWN',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) LUNCZ = 0
      CALL GETWDF(CHHOME)
	PRINT *, 'home = ',CHHOME
      LHOME = IS(1)
#endif
#if defined(CERNLIB_OS9)
      ISOCK=SSETUP(ISKIN,ISKOUT)
      LUNCZ=6
      OPEN(6,'std_out')
      OPEN(7,'std_out')
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Check on logical name SYS$REM_NODE to determine
*     whether we have been started by DECnet or TCP/IP
*
      CALL CZGTLG('SYS$REM_NODE',REMNOD,'LNM$JOB',IRC)
      IF(IRC.NE.0) THEN
         IPROT = 0
         ISOCK=ISETUP(ISKIN,ISKOUT)
         LUNCZ=77
         OPEN(UNIT=77,FILE='ZSERV.LOG',STATUS='NEW',IOSTAT=ISTAT)
         IF(ISTAT.NE.0) LUNCZ = 0
      ELSE
         IPROT = 1
         ISOCK = 0
         LUNCZ = 6
         STATUS = LIB$GET_LUN(LUNDEC)
         IF( .NOT.STATUS ) THEN
            ISOCK = -1
            GO TO 90
         ENDIF
         ISKIN = LUNDEC
         ISKOUT = LUNDEC
         OPEN(UNIT=LUNDEC,FILE='SYS$NET',STATUS='OLD')
*
*     Temporary...
*
         CALL XZLOGL(3)
      ENDIF
#endif
#if defined(CERNLIB_IBM)
      ARGS=' '
      CALL GOPARM(NARG,ARGS)
      IF(NARG.EQ.0)THEN
         IPORT=2345
      ELSE
         NCH=LENOCC(ARGS)
* jds 21/02/92
         IPORT = ICDECI(ARGS,1,NCH)
*        PORT=' '
*        PORT(10-NCH+1:10)=ARGS(1:NCH)
*        READ(PORT,'(I10)')IPORT
      ENDIF
      LUNCZ=6
#endif
#if (!defined(CERNLIB_TCPSOCK))&&(defined(CERNLIB_IBM))
      CALL SSETUP(ISOCK,IPORT)
      ISKIN=ISOCK
      ISKOUT=ISOCK
#endif
#if (defined(CERNLIB_TCPSOCK)) && (defined(CERNLIB_IBM) || defined(CERNLIB_WINNT))
#if defined(CERNLIB_WINNT)
      IPORT = 0
#endif
      ISOCK = SSETUP(ISKIN,ISKOUT,IPORT)
#endif
      IF(ISOCK.LT.0)GO TO 90
*
*           Start ZEBRA
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      IDUMMY = CINIT(DUMMY)
#endif
      CALL HLIMIT(NWPAWC)
      CALL HERMES(LUNCZ)
      CALL HDELET(0)
*
*     Number of RZ files currently open
*
      NCHRZ  = 0
      CALL VZERO(LUNRZ,MAXFIL)
*
*          Open the FZ buffers
*
      LBUF   = 270
 
      LUNFZI = 999
      LUNFZO = 998
 
      CALL FZFILE(LUNFZI,LBUF,'SIC')
      CALL FZHOOK(LUNFZI,CZTCP,DBUF)
      CALL FZFILE(LUNFZO,LBUF,'SOC')
      CALL FZHOOK(LUNFZO,CZTCP,DBUF)
      IADTCP=JUMPAD(CZTCP)
      CALL JUMPST(IADTCP)
*
      IQPRNT = LUNCZ
      IQLOG  = LUNCZ
#if defined(CERNLIB_IBM)
*
*     Allow file transfer of RECFM V files
*
      CALL ERRSET(212,256,-1,1,1)
#endif
#if defined(CERNLIB_OS9)
      CPU='1'
**      MODULE='hb_data'
**      CALL ZSMODU(CPU,MODULE)
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_LOGIN))
      INQUIRE(FILE='LOGIN.COM',EXIST=IEXIST)
      IF(IEXIST) THEN
         IC = SYSTEMF('@LOGIN')
      ENDIF
#endif
*
*     Set units, loglevel
*
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
*
*     Loop on client messages
*
   10 CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 15
#if defined(CERNLIB_DEBUG)
      IF(LUNCZ.NE.0)WRITE(LUNCZ,'(A,A)') 'ZSERV. ',CHMAIL
#endif
      CHMESS=CHMAIL(1:6)
*
*     Close?
*
      IF(CHMESS.EQ.'*CLOSE') GOTO 15
*
*     RSH LOG*OUT
*
      IF(CHMESS.EQ.'MESS :'.AND.
     +   ICNTHU(CHMAIL(7:9),'LOG*OUT',1).NE.0) THEN
         CALL CZPUTA('1 Closing remote connection',ISTAT)
         GOTO 15
      ENDIF
*
*     PIAF
*
      IF(CHMESS.EQ.'PIAF :')THEN
         CALL SZPIAF(CHMAIL(7:80))
         GO TO 10
      ENDIF
*
*           MESSAGE
*
      IF(CHMESS.EQ.'MESS :')THEN
         CALL MESSAGE(CHMAIL(7:80))
         GO TO 10
      ENDIF
*
*           XZIO
*
      IF(CHMESS.EQ.'XZIO :')THEN
         CALL XZSERV(CHMAIL(7:80))
         GO TO 10
      ENDIF
*
*           FATMEN
*
      IF(CHMESS.EQ.'FATM :')THEN
         CALL FATMEN(CHMAIL(7:80))
         GO TO 10
      ENDIF
*
*           CZIN
*
      IF(CHMESS.EQ.'CZIN  ')THEN
         CALL ZSRZIN(CHMAIL)
         GO TO 10
      ENDIF
*
*           CZOUT
*
      IF(CHMESS.EQ.'CZOUT ')THEN
         CALL ZSOUT(CHMAIL)
         GO TO 10
      ENDIF
#if !defined(CERNLIB_NOZFTP)
*
      NCHM=LENOCC(CHMAIL)
      IF(NCHM.LE.0)GO TO 15
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1001)CHMAIL(1:NCHM)
 1001 FORMAT(' Starting ==> ',A)
 1002 FORMAT(' Finishing==> ',A)
*
*           GETA
*
      IF(CHMESS.EQ.'GETA :')THEN
         CALL SGETA(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*           PUTA
*
      IF(CHMESS.EQ.'PUTA :')THEN
         CALL SPUTA(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*           GETRZ
*
      IF(CHMESS.EQ.'GETRZ:')THEN
         CALL SGETRZ(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*           PUTRZ
*
      IF(CHMESS.EQ.'PUTRZ:')THEN
         CALL SPUTRZ(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
*
*           PUTPIAF
*
      IF(CHMESS.EQ.'PPIAF:')THEN
         CALL SPPIAF(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
#endif
*
*           GETFZ
*
      IF(CHMESS.EQ.'GETFZ:')THEN
         CALL SGETFZ(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*           PUTFZ
*
      IF(CHMESS.EQ.'PUTFZ:')THEN
         CALL SPUTFZ(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*           GETP
*
      IF(CHMESS.EQ.'GETP :')THEN
         CALL SGETP(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*           PUTP
*
      IF(CHMESS.EQ.'PUTP :')THEN
         CALL SPUTP(CHMAIL(7:80))
         IF(LUNCZ.NE.0)WRITE(LUNCZ,1002)CHMAIL(1:NCHM)
         GO TO 10
      ENDIF
*
*          No options found. Close server socket
*          Wait for another client
*
   15 CONTINUE
      DO 20 I=1,NCHRZ
#if defined(CERNLIB_OS9)
         IF(LUNRZ(I).EQ.0)CALL F_UNLNK(CHRZ(I))
#endif
         IF(LUNRZ(I).GT.0)THEN
            CALL RZEND(CHRZ(I))
            CLOSE (LUNRZ(I))
         ENDIF
   20 CONTINUE
      NCHRZ=0
      IF(LUNCZ.NE.0)WRITE(LUNCZ,*) 'CLOSE request received. '
      IF(LUNCZ.NE.0)WRITE(LUNCZ,1003)
 1003 FORMAT(' Attached files released. Connection closed.')
#if defined(CERNLIB_UNIX)
      ISTAT=ICLOSE(ISKIN)
#endif
#if defined(CERNLIB_OS9)
      ISTAT=SCLOSE(ISKIN)
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.EQ.0) THEN
         ISTAT = ICLOSE(ISKIN)
      ELSE
         ISTAT = 0
      ENDIF
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      ISTAT = SCLOSE(ISKIN)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
      CALL SCLOSE(ISKIN)
      ISTAT=0
#endif
*
   90 CONTINUE
      STOP
      END
