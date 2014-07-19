*
* $Id: rlogin.F,v 1.3 1996/03/29 15:48:10 cernlib Exp $
*
* $Log: rlogin.F,v $
* Revision 1.3  1996/03/29 15:48:10  cernlib
* Remove the tabs! These are replaced by a single space by cpp.
*
* Revision 1.2  1996/03/15 10:29:34  dinofm
* RLOGIN subroutine has been modified so that once the connection to PIAF is up,
* the PIAF master is queried about his version using the PIAF MESSAGE command
* back door.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/08 09/06/94  16.30.23  by  Fons Rademakers
*-- Author :
      SUBROUTINE RLOGIN
*
*           To start a communication with a remote computer
*           Current implementation based on TCP/IP
*
#if defined(CERNLIB_CZ)
#include "hbook/hcdire.inc"
#include "hbook/czsock.inc"
#include "paw/quest.inc"
#include "paw/pcpatl.inc"
#include "hbook/hcpiaf.inc"
#include "paw/pcpiaf.inc"
#include "paw/pcwk.inc"
*
      CHARACTER*32 CHHOST, CHTABL
      LOGICAL      TOPIAF
      CHARACTER*16 CHPORT, CHCLUS, CHUSER, CHVERS
*
*=========================================================
*
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'CONNECT') THEN
         TOPIAF=.TRUE.
         CHPORT='piafserv'
         CALL KUGETS(CHCLUS,NCH)
         IF(NCH.LE.0)GO TO 99
         CALL CUTOL(CHCLUS)
         CALL KUGETS(CHHOST,NCH)
         IF(NCH.LE.0)GO TO 99
         CALL CUTOL(CHHOST)
         ISPROTO = IPROTO
*
         IF (NPAR.GE.3) THEN
*-- developer's backdoor
            CALL KUGETS(CHVERS,N)
            IF (NPAR.GE.4) THEN
               CALL KUGETS(CHPORT,N)
               IF (NPAR .GE. 5) THEN
                  CALL KUGETI(ISPROTO)
               ENDIF
            ENDIF
         ELSE
            CHVERS=VERSPF
         ENDIF
         CHTABL=CHCLUS
      ELSE
         TOPIAF=.FALSE.
         CHPORT='pawserv'
         CALL KUGETS(CHHOST,NCH)
         IF(NCH.LE.0)GO TO 99
         CALL CUTOL(CHHOST)
         CHTABL=CHHOST
      ENDIF
*
      IF(NCHTOP.GE.20)THEN
         PRINT *,' Too many communications: cannot connect'
         GO TO 99
      ENDIF
*
      DO 10 I=1,NCHTOP
         IF(ICHTOP(I).LT.1000)GO TO 10
         IF(CHTABL.EQ.HFNAME(I)(15:46))THEN
            PRINT *,' Communication already established'
            GO TO 99
         ENDIF
  10  CONTINUE
*
      IF (TOPIAF) THEN
         PIAFRC = ' '
         CALL PZOPEN(CHPORT,CHCLUS,CHHOST,CHVERS,PIAFRC,ISTAT)
         IF (ISTAT .NE. 0) GOTO 99
*
*-- receive piafserver startup confirmation
*
         CALL CZGETA(CHSMPF,ISTAT)
         IF (ISTAT .NE. 0) GOTO 99
         L=LENOCC(CHSMPF)
         IF(CHSMPF(1:2).EQ.'OK') THEN
            PRINT *, CHSMPF(4:L)
         ELSE
            PRINT *, CHSMPF(1:L)
            GOTO 99
         ENDIF
*
*-- send user account name, version id, workstation type and protocol level
*
         CALL KUSERID(CHUSER)
         CALL CUTOL(CHUSER)
         WRITE(CHSMPF,'(2A16,2I12)') CHUSER, CHVERS, IWK, ISPROTO
         CALL CZPUTA(CHSMPF,ISTAT)
         IF (ISTAT .NE. 0) GOTO 99
*
*-- send userpass information that will be used by the master
*-- to start the slaves
*
         IF (ISPROTO .GE. 1) THEN
            WRITE(CHSMPF,'(A64)') PIAFRC
#endif
#if (defined(CERNLIB_CZ))&&(!defined(CERNLIB_IBM))
            CALL CZPUTA(CHSMPF,ISTAT)
#endif
#if (defined(CERNLIB_CZ))&&(defined(CERNLIB_IBM))
            CALL CZPUTB(ISKIN,CHSMPF,LENOCC(CHSMPF),ISTAT)
#endif
#if defined(CERNLIB_CZ)
            IF (ISTAT .NE. 0) GOTO 99
         ENDIF
*
*-- get motd message
*
         IQUEST(1) = 0
         CALL PFLOOP(ISTAT)
*
*-- enable exception signaling from client to server
*
         CALL PZSYNC(ISKIN)
      ELSE
         CALL CZOPEN(CHPORT,CHHOST,ISTAT)
      ENDIF
      IF(ISTAT.NE.0 .OR. IQUEST(1).NE.0)THEN
         IQUEST(1)=1
         PRINT *,' Cannot open communication with ',CHHOST
         GO TO 99
      ENDIF
*
*
      NCHTOP=NCHTOP+1
      CHTOP(NCHTOP)=CHHOST(1:16)
      CALL CLTOU(CHTOP(NCHTOP))
      ICHTOP(NCHTOP)=ISKIN+10000*ISKOUT
      ICHLUN(NCHTOP)=0
      HFNAME(NCHTOP)='Connection to '//CHTABL
      IF(TOPIAF) THEN
         ICHTYP(NCHTOP)=2
         CONNPF=.TRUE.
         CLINPF=ISKIN
         CLUTPF=ISKOUT

*-- Retrieve on the client side the version of Piaf server (0...n)
         CALL KUEXEC('PIAF/MESSAGE PQRY VERS')
         IF (ISTAT.NE.0) THEN
            PRINT *,' Cannot query PIAF server on ',CHHOST
            GO TO 99
         ENDIF

*
*-- send initial state
*
         CALL PFSTATE
      ELSE
         ICHTYP(NCHTOP)=0
         CALL HCDIR('//'//CHTOP(NCHTOP),' ')
      ENDIF
*
#endif
#if !defined(CERNLIB_CZ)
      PRINT *,' PAW not compiled with communication option'
#endif
*
  99  END
