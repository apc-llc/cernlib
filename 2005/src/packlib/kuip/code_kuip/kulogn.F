*
* $Id: kulogn.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kulogn.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.01/06 29/03/93  16.27.32  by  Alfred Nathaniel
*-- Author :    Jamie Shiers
      SUBROUTINE KULOGN(CHPACK,CHOPT)

      CHARACTER*(*) CHPACK,CHOPT
      CHARACTER*1   CHOPTT
      CHARACTER*8   CHVAR
      CHARACTER*255 CHPATH
      COMMON/SLATE/ IS(40)
*
*     Execute logon kumacs for package 'CHPACK'
*     KUMACs are xxxSYS, xxxGRP, xxxUSR and xxxLOGON
*     selected in CHOPT R='S,G,U,L,V' ('V'=verbose flag)
*
*     VAX/VMS : look in directories defined by search list xxxPATH
*               if xxxPATH not defined, use SYS$LOGIN,SYS$DISK:[]
*               (i.e. current and home directories)
*     Unix    : look in path xxx PATH
*               if xxxPATH not defined, use current and home directories
*     VM/CMS  : check disks in xxxPATH. if not defined, use A
*
*     MVS     : use prefix
*
      LPACK = LENOCC(CHPACK)
      LOPT  = LENOCC(CHOPT)

      IOPTS = INDEX(CHOPT(1:LOPT),'S')
      IOPTG = INDEX(CHOPT(1:LOPT),'G')
      IOPTU = INDEX(CHOPT(1:LOPT),'U')
      IOPTL = INDEX(CHOPT(1:LOPT),'L')
      IOPTV = INDEX(CHOPT(1:LOPT),'V')

      CHOPTT = ' '
      IF(IOPTV.NE.0) CHOPTT = 'V'

#if defined(CERNLIB_IBMMVS)
*
*     Get prefix
*
      CALL KPREFI(CHPATH,LPATH)
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_UNIX)
*
*     Translate path variable
*
      CHVAR = CHPACK(1:LPACK) // 'PATH'
      CALL GETENVF(CHVAR,CHPATH)
      LPATH = IS(1)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_MSDOS))
*
*     Restrict search to current and home directory
*     if variable not defined
*
      IF(LPATH.EQ.0) THEN
         CHPATH(1:2) = '.:'
         CALL GETENVF('HOME',CHPATH(3:))
         LPATH = IS(1) + 2
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
*
*     Restrict search to A disk if variable not defined
*
      IF(LPATH.EQ.0) THEN
         CHPATH = 'A'
         LPATH  = 1
      ENDIF
#endif
#if defined(CERNLIB_VMS)
*
*     Logical name is a search list
*
      CHPATH = CHPACK(1:LPACK) // 'PATH'
#endif
*
*     Execute system logon
*
      IF(IOPTS.NE.0) THEN
         CHVAR = CHPACK(1:LPACK) // 'SYS'
         CALL KILOGN(CHPATH,CHVAR,CHOPTT)
      ENDIF
*
*     Execute group logon
*
      IF(IOPTG.NE.0) THEN
         CHVAR = CHPACK(1:LPACK) // 'GRP'
         CALL KILOGN(CHPATH,CHVAR,CHOPTT)
      ENDIF
*
*     Execute user logon
*
      IF(IOPTU.NE.0) THEN
         CHVAR = CHPACK(1:LPACK) // 'USR'
         CALL KILOGN(CHPATH,CHVAR,CHOPTT)
      ENDIF
*
*     Execute local logon
*
      IF(IOPTL.NE.0) THEN
         CHVAR = CHPACK(1:LPACK) // 'LOGON'
         CALL KILOGN(CHPATH,CHVAR,CHOPTT)
      ENDIF

      END

