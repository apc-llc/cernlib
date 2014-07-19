*
* $Id: cssedt.F,v 1.3 1997/03/14 11:58:56 mclareni Exp $
*
* $Log: cssedt.F,v $
* Revision 1.3  1997/03/14 11:58:56  mclareni
* WNT mods
*
* Revision 1.2  1996/10/09 12:30:13  cernlib
* For Linux add an external for system. This helps g77 to compile.Z
*
* Revision 1.1.1.1.2.1  1997/01/21 11:34:25  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/26 17:16:20  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 09/11/94  19.41.35  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSSEDT(FILE)
***----------------------------------
*     invokes host editor
***----------------------------------
*
      CHARACTER*(*) FILE
*
#if defined(CERNLIB_APOLLO)
%INCLUDE '/sys/ins/pad.ins.ftn'
      INTEGER*2 WINDOW(4),L,ONE,SID
      PARAMETER (ONE=1)
      DATA WINDOW /2*20,2*0/
#endif
#if defined(CERNLIB_VAX)
      CHARACTER*(*) CMD
      PARAMETER (CMD='$ EDIT/EDT')
      INTEGER EDT$EDIT
#endif
#if defined(CERNLIB_IBM)
      CHARACTER*16  CNAME
#endif
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_LINUX)||defined(CERNLIB_WINNT)||defined(CERNLIB_DECS)
      character*64 editor,EDTFIL
      integer systemf
#endif
#if defined(CERNLIB_APOLLO)
      L=LEN(FILE)
      CALL PAD_$CREATE_WINDOW(FILE,L,PAD_$EDIT,ONE,WINDOW,SID,IST)
      IF (IST.NE.0) THEN
        PRINT *,'CSSEDT: System failure creating editor'
        FILE(1:1)='@'
        GO TO 10
      ENDIF
      CALL PAD_$EDIT_WAIT(SID,IST)
      IF (IST.NE.0) PRINT *,'CSSEDT: System failure waiting for editor'
10    CALL STREAM_$CLOSE(SID,IST)
      IF (IST.NE.0) PRINT *,'CSSEDT: System failure closing stream'
#endif
#if defined(CERNLIB_VAX)
C      IST=LIB$SPAWN(CMD//' '//FILE)
      IST=EDT$EDIT(FILE)
      IF (.NOT.IST) THEN
        FILE(1:1)='@'
        PRINT *,'CSSEDT: System failure creating editor'
      ENDIF
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CNAME=FILE
      CALL VMCMS('XEDIT '//CNAME,IRET)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUEDIT ( FILE, IRET )
#endif
#if defined(CERNLIB_IBM)
      IF(IRET.NE.0) THEN
        FILE(1:1)='@'
        PRINT *,'CSSEDT: System problem editing workfile'
      ENDIF
#endif
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_LINUX)||defined(CERNLIB_WINNT)||defined(CERNLIB_DECS)
      EDTFIL = FILE(1:MIN(64,LENOCC(FILE)))
      EDITOR = ' '
      CALL GETENVF('EDITOR',EDITOR)
#endif
#if defined(CERNLIB_MSDOS)
      IF(EDITOR.EQ.' ') EDITOR='EDIT'
#endif
#if defined(CERNLIB_LINUX)||defined(CERNLIB_DECS)
      IF(EDITOR.EQ.' ') EDITOR='vi'
#endif
#if defined(CERNLIB_WINNT)
      IF(EDITOR.EQ.' ') EDITOR='NOTEPAD'
#endif
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_LINUX)||defined(CERNLIB_WINNT)||defined(CERNLIB_DECS)
      IRET = SYSTEMF(EDITOR(1:LENOCC(EDITOR))//' '
     *              //EDTFIL(1:LENOCC(EDTFIL)))
C      CALL KUEDIT(FILE, IRET)
      IF (IRET .NE. 0) THEN
        FILE(1:1)='@'
        PRINT *,'CSSEDT: System problem editing workfile'
      END IF
#endif
#if defined(CERNLIB_HPUX)
      CALL KUEDIT ( FILE, IRET )
#endif
      END
