*
* $Id: rztof.F,v 1.1.1.1 1996/03/06 10:47:19 mclareni Exp $
*
* $Log: rztof.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:19  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
*
*     Fix call to RZOPEN for IBM systems - remove trailing blank
*     left over from pre-RZOPEN days
*
#if defined(CERNLIB_RTOX)
      PROGRAM RTOX
#endif
#if defined(CERNLIB_RTOA)
      PROGRAM RTOA
#endif
C
C
#include "rzpawc.inc"
#include "rzlenf.inc"
      COMMON / QUEST / IQUEST(100)
      COMMON / PAWC  / HMEMOR(MEMOR)
      COMMON /RZCKEY / IHEAD(3),KEY(100),KEY2(100),KEYDUM(50)
#include "zunit.inc"
C
      CHARACTER*(LENF) FIN, FOUT
      INTEGER      OURECL
#if defined(CERNLIB_QMIBM)
      INTEGER      SYSTEMF
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))
      LOGICAL      GET_COMMAND_LINE
      CHARACTER*4  CHVERB
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RTOX))
      EXTERNAL     RTOX_CLD
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RTOA))
      EXTERNAL     RTOA_CLD
#endif
#include "rzmode.inc"
C
#if defined(CERNLIB_QMIBM)
      CALL ERRSET(151,0,-1,1,1)
#endif
      IQPRNT = 6
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RTOX))
      CHVERB = 'RTOX'
      IF ( .NOT. GET_COMMAND_LINE( RTOX_CLD , CHVERB ) ) THEN
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RTOA))
      CHVERB = 'RTOA'
      IF ( .NOT. GET_COMMAND_LINE( RTOA_CLD , CHVERB ) ) THEN
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))
        PRINT 9001,CHVERB
9001  FORMAT(1X,A,'. invalid command line')
        CALL EXIT
      ENDIF
#endif
      CALL GETF(FIN,FOUT,'RZ','F'//MODE)
C
#if (defined(CERNLIB_RTOX))&&(defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT))
      OURECL = 3600
#endif
#if (defined(CERNLIB_RTOX))&&(defined(CERNLIB_QMIBM)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMALT)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIBX))
      OURECL = 3600
#endif
#if (defined(CERNLIB_RTOX))&&(defined(CERNLIB_QMVAX)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMVMI))
      OURECL =  900
#endif
#if (defined(CERNLIB_RTOA))&&(defined(CERNLIB_QMIBM)||defined(CERNLIB_QMVAX)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMUIX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT))
      OURECL =   80
#endif
  10  LRECL  = 0
#if defined(CERNLIB_QMIBM)
      CALL RZOPEN(1,'//RZ',FIN(2:),' ',LRECL,ISTAT)
#endif
#if !defined(CERNLIB_QMIBM)
      CALL RZOPEN(1,'//RZ',FIN,' ',LRECL,ISTAT)
#endif
      IF(ISTAT.NE.0) STOP
#if (defined(CERNLIB_RTOX))&&(defined(CERNLIB_QMIBM))&&(defined(CERNLIB_IOPACK))
      IRET=SYSTEMF('FILEDEF IOFILE02 DISK '//FOUT//
     +           ' (RECFM U BLOCK 3600')
#endif
#if (defined(CERNLIB_RTOX))&&(defined(CERNLIB_QMIBM))&&(!defined(CERNLIB_IOPACK))
      CALL FILEINF (IRET,'RECFM','U','LRECL',3600)
      FOUT(1:1)='/'
#endif
#if (defined(CERNLIB_RTOA))&&(defined(CERNLIB_QMIBM))
      CALL FILEINF (IRET,'RECFM','F','LRECL',  80)
      FOUT(1:1)='/'
#endif
#if (defined(CERNLIB_QMIBM)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMUIX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX))&&(!defined(CERNLIB_QMALT))
      OPEN(2,FILE=FOUT,FORM=FORM,            STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMALT)||defined(CERNLIB_QMAPO)
      OPEN(2,FILE=FOUT,FORM=FORM,RECL=OURECL,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMVAX)
      OPEN(2,FILE=FOUT,FORM=FORM,RECL=OURECL,STATUS='NEW',
     +     RECORDTYPE='FIXED',
#endif
#if (defined(CERNLIB_QMVAX))&&(defined(CERNLIB_RTOX))
     +     BLOCKSIZE=OURECL*4,
     +     CARRIAGECONTROL='NONE')
#endif
#if (defined(CERNLIB_QMVAX))&&(defined(CERNLIB_RTOA))
     +     CARRIAGECONTROL='LIST')
#endif
C
      CALL RZTFDO(MODE)
C
      CLOSE(1)
#if defined(CERNLIB_QMAPO)||defined(CERNLIB_QMVAX)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMUIX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)
      CLOSE(2)
#endif
#if (defined(CERNLIB_QMIBM))&&(!defined(CERNLIB_IOPACK)||defined(CERNLIB_RTOA))
      CLOSE(2)
#endif
#if (defined(CERNLIB_QMIBM))&&(defined(CERNLIB_IOPACK))&&(defined(CERNLIB_RTOX))
      IRET=SYSTEMF('FILEDEF IOFILE02 CLEAR')
#endif
C
      END
