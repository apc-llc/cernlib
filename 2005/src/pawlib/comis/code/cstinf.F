*
* $Id: cstinf.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: cstinf.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.17/07 16/12/93  18.28.21  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSTINF(FILENM)
***--------------------------
*     moves text from page-memory into file
***--------------------------
      CHARACTER *(*) FILENM
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/csrec.inc"
#include "comis/cslun.inc"
#if defined(CERNLIB_VAX)
      OPEN(LUNLIB,FILE=FILENM,STATUS='NEW',ERR=97
     + ,RECORDTYPE='VARIABLE',ORGANIZATION='SEQUENTIAL',
     +  CARRIAGECONTROL='LIST')
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      OPEN(LUNLIB,FILE=FILENM,STATUS='UNKNOWN',ERR=97)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CHARACTER LUNVM*2,LOCFIL*32
      WRITE(LUNVM,'(I2)')LUNLIB
      LOCFIL=FILENM
      CALL VMCMS('FILEDEF '//LUNVM//' DISK '//LOCFIL//
     + ' ( RECFM F LRECL 80',IRET)
      IF(IRET.NE.0)GO TO 97
      OPEN(LUNLIB,STATUS='UNKNOWN',ERR=97)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN ( LUNLIB, FILENM, 'UNKNOWN', ISTAT )
      IF ( ISTAT .NE. 0 )               GO TO 97
#endif
      REWIND LUNLIB
      J=JPMB
 1    IF(J.EQ.0)GO TO 2
      CALL CSGETL(J,REC1,N,M)
#if defined(CERNLIB_VAX)||defined(CERNLIB_UNIX)
       WRITE(LUNLIB,77)REC1(1:N)
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_UNIX))
       WRITE(LUNLIB,77)REC1
#endif
      GO TO 1
*---------------------
 77   FORMAT(A)
 2    ENDFILE LUNLIB
      CLOSE(LUNLIB)
      CSTINF=1
      RETURN
 97   REC1='CSTINF: file '//FILENM//' was not open'
      CALL CSSOUT(REC1)
      CSTINF=0
      END
