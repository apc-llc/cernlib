*
* $Id: czopen.F,v 1.2 2005/04/18 15:23:08 mclareni Exp $
*
* $Log: czopen.F,v $
* Revision 1.2  2005/04/18 15:23:08  mclareni
* Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
*
* Revision 1.1.1.1  1996/03/08 15:44:23  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZOPEN(CHSERV,HOST,ISTAT)
*
*****************************************************************
*                                                               *
*          The PAW/ZEBRA communication package utility          *
*          Current implementation based on TCP/IP               *
*                                                               *
*            Author: Rene Brun  CERN/DD                         *
*                 with the collaboration of Ben Segal (DD)      *
*                 for the interface with the TCPAW (C) system   *
*                                                               *
*          To open socket with remote host HOST                 *
*                                                               *
*****************************************************************
*
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
      COMMON/CDBUF/DBUF(10)
      CHARACTER*(*) HOST,CHSERV
*
*           Need to copy HOST to HOST1 for passing correctly
*           the parameter HOST to the C routine
*           Apollo bug with CHARACTER*(*) and C !!!
*
      CHARACTER*80 HOST1
      INTEGER CSETUP
#if !defined(CERNLIB_IBM)
      CHARACTER*20 SERVICE
#endif
#if defined(CERNLIB_IBM)
      CHARACTER*80 SERVICE
#endif
#if (defined(CERNLIB_APOLLO))&&(defined(CERNLIB__LOG))
      CHARACTER*26 CHFILE
#endif
      EXTERNAL CZTCP
 
      LUNFZI = 999
      LUNFZO = 998
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.EQ.1) THEN
         CALL CZDOPN(CHSERV,HOST,ISTAT)
         RETURN
      ENDIF
#endif
*
      IF(IADTCP.EQ.0)THEN
#if defined(CERNLIB_VAXVMS)
*
*            Setup of C run time
*
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_QCDEC))
         CALL VAXC$CRTL_INIT
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_QCDEC))
         CALL DECC$VAXC$CRTL_INIT
#endif
         LBUF=270
         CALL FZFILE(LUNFZI,LBUF,'SIOC')
         CALL FZHOOK(LUNFZI,CZTCP,DBUF)
         CALL FZFILE(LUNFZO,LBUF,'SOC')
         CALL FZHOOK(LUNFZO,CZTCP,DBUF)
         IADTCP=JUMPAD(CZTCP)
         CALL JUMPST(IADTCP)
      ENDIF
*
#if !defined(CERNLIB_APOLLO)
      LUNCZ=6
#endif
#if (defined(CERNLIB_APOLLO))&&(defined(CERNLIB__LOG))
      LUNCZ=77
      CHFILE=' '
      CALL DATIME(ND,NT)
      WRITE(CHFILE,1000)ND,NT
 1000 FORMAT('/var/log/cz',I6,I4,'.log')
      DO 10 I=11,23
         IF(CHFILE(I:I).EQ.' ')CHFILE(I:I)='0'
  10  CONTINUE
      OPEN(UNIT=LUNCZ,FILE=CHFILE,STATUS='UNKNOWN')
#endif
      SERVICE=CHSERV
      HOST1=HOST
*
      CALL CUTOL(SERVICE)
#if !defined(CERNLIB_IBM)||defined(CERNLIB_TCPSOCK)
      ISOCK  = CSETUP(HOST1,ISKIN,ISKOUT,SERVICE)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_TCPSOCK))
      CALL CSETUP(HOST1,ISOCK,SERVICE)
      ISKIN  = ISOCK
      ISKOUT = ISOCK
#endif
      IF(ISOCK.LT.0)THEN
         ISTAT=IABS(ISOCK)
      ELSE
         ISTAT=0
      ENDIF
 
*
      END
