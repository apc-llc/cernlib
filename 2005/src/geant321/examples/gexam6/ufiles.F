*
* $Id: ufiles.F,v 1.1.1.1 1995/10/24 10:22:19 cernlib Exp $
*
* $Log: ufiles.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UFILES
*
*            To open FFREAD and HBOOK files
*
#if defined(CERNLIB_CRAY)
      OPEN(UNIT=4,FILE='gexam6.dat',STATUS='OLD')
      OPEN(UNIT=34,FILE='gexam6.hist',ACCESS='DIRECT',
     +     RECL=8192,STATUS='NEW',FORM='UNFORMATTED')
#endif
#if defined(CERNLIB_APOLLO)
      OPEN(UNIT=4,FILE='gexam6.dat',STATUS='OLD')
      OPEN(UNIT=34,FILE='gexam6.hist',ACCESS='DIRECT',
     +     RECL=4096,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)
      OPEN(UNIT=4,FILE='GEXAM6.DAT',STATUS='UNKNOWN')
      OPEN(UNIT=34,FILE='GEXAM6.HIST',ACCESS='DIRECT',
     +     FORM='UNFORMATTED',RECL=1024,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_IBM)
      CALL VMCMS('FILEDEF  4 DISK GEXAM6 DAT',IRET)
      CALL VMCMS('FILEDEF 34 DISK GEXAM6 HIST A6',IRET)
      OPEN(UNIT=4,STATUS='UNKNOWN')
      OPEN(UNIT=34,ACCESS='DIRECT',
     +     FORM='UNFORMATTED',RECL=4096,STATUS='UNKNOWN')
#endif
      END
