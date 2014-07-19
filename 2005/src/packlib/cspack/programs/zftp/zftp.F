*
* $Id: zftp.F,v 1.2 1996/09/20 12:45:16 cernlib Exp $
*
* $Log: zftp.F,v $
* Revision 1.2  1996/09/20 12:45:16  cernlib
* changes for linux around usage of iargc
*
* Revision 1.1.1.1  1996/03/08 15:44:18  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      PROGRAM ZFTP
********************************************************************
*                                                                  *
*          ZEBRA File Transfer Program using TCP/IP                *
*          ========================================                *
*                                                                  *
*         Current implementation on Apollo, VAX/VMS and IBM/VM     *
*                                                                  *
*          The following commands are available:                   *
*     OPEN : to open a communication with a remote machine         *
*     CLOSE: to close communication with a remote machine          *
*     GETA : to transfer a text file from a remote machine         *
*     PUTA : to transfer a text file to a remote machine           *
*     GETB : to transfer a binary file from a remote machine       *
*     PUTB : to transfer a binary file to a remote machine         *
*     GETP : to transfer a binary PAM from a remote machine        *
*     PUTP : to transfer a binary PAM to a remote machine          *
*     GETRZ: to transfer a remote RZ file from a remote machine    *
*     PUTRZ: to transfer a local RZ file to a remote machine       *
*     RSHEL: to send a message to the remote machine               *
*     LS   : to list a remote directory                            *
*     VERS : to print current version                              *
*                                                                  *
*          Note that the small communication packages CZ (Fortran) *
*          and TCPAW (C) must be linked with ZFTP.                 *
*    needs on vax files sgtty.h and pwd.h                          *
*    $SET PROT=(S,O:RWD,G,W) FTPLOGIN. (Set file protection)       *
*    /bin/chmod 0600 .netrc                                        *
*                                                                  *
*            Author: Rene Brun CERN/DD                             *
*                  in collaboration with :                         *
*                             B.Segal (DD) for TCPAW               *
*                                                                  *
********************************************************************
#if defined(CERNLIB_OS9)
#include "cspack/hos9c.inc"
#endif
#include "cspack/czsock.inc"
#include "cspack/pawc.inc"
      CHARACTER*100 CHARGS
      EXTERNAL ZEXIT
*
      IADTCP=0
      ISKIN=-1
      ISKOUT=-1
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      IDUMMY = CINIT(DUMMY)
#endif
      CALL HLIMIT(NWPAWC)
      CALL KUINIT(30000)
      CALL KUEXIT(ZEXIT)
      CALL KUQUIT(ZEXIT)
*
      CALL FTPDEF
*
*     Get any command line options
*
      CALL CFILL(' ',CHARGS,1,100)
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_GSI))
      CALL PALOG('ZFT',1)
#endif
#if defined(CERNLIB_IBM)
      CALL GOPARM(LPARM,CHARGS)
      CALL TIMEST(1000.)
#endif
#if defined(CERNLIB_VAXVMS)
      IFLAG = 0
      CALL LIB$GET_FOREIGN(CHARGS,,LPARM,IFLAG)
#endif
#if defined(CERNLIB_CRAY)
      IF(IARGC(DUMMY).NE.0) LPARM = IARGV(1,CHARGS)
#endif
#if (defined(CERNLIB_UNIX)) && (!defined(CERNLIB_CRAY))\
  &&(!defined(CERNLIB_HPUX)) && (!defined(CERNLIB_LINUX))
      IF(IARGC(DUMMY).NE.0) CALL GETARG(1,CHARGS)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(defined(CERNLIB_HPUX))
      LPARM = IGETARG(1,CHARGS,LEN(CHARGS))
#endif
#if defined(CERNLIB_LINUX)
      IF(IARGC().NE.0) CALL GETARG(1,CHARGS)
#endif
      LPARM = LENOCC(CHARGS)
      IF(LPARM.NE.0) THEN
         CALL KUEXEC('OPEN '//CHARGS(1:LPARM))
      ENDIF
*
*     Change default prompt
*
      CALL KIPRMT('ZFTP> ')
*
*     Initialise I/O units and debug level
*
      IPRINT = 6
      LOGLVL = 0
      LUNI   = 1
      LUNO   = 2
      CALL XZINIT(IPRINT,LOGLVL,LUNI,LUNO)
*
      CALL KUWHAT
*
      CALL ZEXIT
#if !defined(CERNLIB_IBM)
*
*-- delete scratch files
*
      CALL KUINQF('LAST.KUMAC',LL)
      IF (LL .GE. 0) THEN
         OPEN(1,FILE='last.kumac',STATUS='UNKNOWN')
         CLOSE(1,STATUS='DELETE')
      ENDIF
      CALL KUINQF('LAST.KUMACOLD',LL)
      IF (LL .GE. 0) THEN
         OPEN(1,FILE='last.kumacold',STATUS='UNKNOWN')
         CLOSE(1,STATUS='DELETE')
      ENDIF
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_GSI))
      CALL PALOG('ZFT',2)
#endif
      END
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE CTL$GL_PCB
      ENTRY CTL$T_ACCOUNT
      ENTRY CTL$T_USERNAME
      END
#endif
