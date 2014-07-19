*
* $Id: cstfrf.F,v 1.1.1.1 1996/02/26 17:16:20 mclareni Exp $
*
* $Log: cstfrf.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:20  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 18/01/94  08.58.56  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSTFRF(FILENM)
***----------------------------------
*        reads text from file and puts one into d-memory
***----------------------------------
      CHARACTER *(*) FILENM
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
#include "comis/csrec.inc"
*+SEQ,CSKEYS.
#include "comis/cslun.inc"
#if defined(CERNLIB_VAX)
      OPEN(LUNLIB,FILE=FILENM,STATUS='OLD',ERR=98
     + ,RECORDTYPE='VARIABLE',ORGANIZATION='SEQUENTIAL',
     +  CARRIAGECONTROL='LIST')
#endif
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      OPEN(LUNLIB,FILE=FILENM,STATUS='OLD',ERR=98)
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CHARACTER LUNVM*2,LOCFIL*32
      WRITE(LUNVM,'(I2)')LUNLIB
      LOCFIL=FILENM
      CALL VMCMS('FILEDEF '//LUNVM//' DISK '//LOCFIL//
     + ' ( RECFM F LRECL 80',IRET)
      IF(IRET.NE.0)GO TO 98
      OPEN(LUNLIB,STATUS='OLD',ERR=98)
#endif
#if defined(CERNLIB_IBMMVS)
      CALL KUOPEN ( LUNLIB, FILENM, 'UNKNOWN', ISTAT )
      IF ( ISTAT .NE. 0 )               GO TO 98
#endif
      REWIND LUNLIB
      IF(JPMB.NE.0)CALL CSLFRE(JPMB)
      J=0
 11   READ(LUNLIB,77,END=99)REC1
      NC=MCLINE(REC1)
      CALL CSPUTL(J,REC1,NC,0)
      IF(JPMB.EQ.0)JPMB=J
      GO TO 11
 77   FORMAT(A)
 98   REC1='CSTFRF:sysedt- file '//FILENM//' was not reopen'
      CALL CSSOUT(REC1)
      CSTFRF=0
      RETURN
 99   CONTINUE
      CLOSE(LUNLIB)
*      JPMC=JPMB
      CSTFRF=1
      END
