*
* $Id: sigini1.F,v 1.1.1.1 1995/12/12 14:36:21 mclareni Exp $
*
* $Log: sigini1.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :          25/02/94  16.30.48  by  Rene Brun
*-- Author :
      SUBROUTINE SIGINI1
C
C
C    ..................................................
C         PURPOSE
C            TO INITIALIZE SIGMA
C
C         USAGE
C            CALL SIINIT
C
C         COMM. BLOCKS USED
C            COM1
C
C         AUTHOR.   C.E.VANDONI          DATE 03/12/73
C
C
C
C
C    ..................................................
           COMMON/SICIFI/ISYSF
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/siclin.inc"
#include "sigma/sicst4.inc"

C
C
        SAVE IFMZ
        DATA IFMZ/0/
       ENTRY SIINIT
C
      CALL MZFORM('VEC','-I',IOVEC)
      CALL MZFORM('VECN','9H 1I -F',IOVECN)
C
       LINE=1

C---  INITALIZE TYPE CHAR*1
      ISYSF=0
      ITAM=6
C---  MISSING INDEX
      MISIDX=888
C---  INITIALIZE MAINKONTROL FOR !PRINT,!PLOT,!TRACE,DISPLAY,ETC.
      DO 34 I=1,6
      MAINKT(I)=0
  34  CONTINUE
C
C     SET NOT AUTOMATIC MODE OF EXECUTION.
C
C     BEGINNING VALUE OF GENERATED LABELS.
      NPRINT=6
C
C     BEGINNING VALUE OF LABELS STACK POINTER.
      LSTAPO = 1
C
      DO 2 I=1,14
      KOLTEX(I)='        '
 2    CONTINUE
C
C
       IF (IFMZ.EQ.0) THEN
        CALL MZLINK(IXPAWC,'/SICLIN/',LA1,LA1,LA9)
        IFMZ=1
         ENDIF
      DO 9 I=1,260
      IPROGR(I)=777777
 9    CONTINUE
C
C
      IPOINT = 1
      ICMPI=1
      ISTAPO=0
      ITYPE=0
      ISTRI=0
C
C  ENTER SIGMA !LARGE
C
      MODE=1
C
C
C---  INITIALIZE SITRAK FOR TRACING   (GENERAL TRACING IS SITRAK(20)=1)
      DO 33 I=1,60
      SITRAK(I)=0
  33  CONTINUE
C
      I8DGT=7
      I8LEN=70
      CNAME='        '
      IT2NAM=111111
C
 999  END
