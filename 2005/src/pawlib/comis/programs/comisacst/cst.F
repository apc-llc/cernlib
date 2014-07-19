*
* $Id: cst.F,v 1.1.1.1 1996/02/26 17:16:38 mclareni Exp $
*
* $Log: cst.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:38  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      PROGRAM CST
      PARAMETER (NST=2000, NCT=2000)
      CHARACTER *8 COM(NCT), SUB(NST)
      CHARACTER *80 FL
      COMMON/COMIS/IA(100)
      COMMON/CSPNTS/MRECU(50),JTOPA
#if defined(CERNLIB_IBM)
      CHARACTER *80 VM,VM1*16,VM2*16
      DATA VM1/'FILEDEF 1 DISK'/, VM2/'FILEDEF 2 DISK'/
#endif
      JTOPA=MJWORD(IA(1))-1
       CALL CSSINP(FL,'Input file:',N)
#if defined(CERNLIB_VAX)
       OPEN(1,FILE=FL(1:N),STATUS='OLD',ERR=10)
#endif
#if defined(CERNLIB_APOLLO)
       OPEN(1,FILE=FL(1:N),STATUS='UNKNOWN',ERR=10)
#endif
#if defined(CERNLIB_IBM)
        VM=VM1//FL(1:N)//' ( RECFM F LRECL 80'
        CALL VMCMS(VM,IERR)
        IF(IERR.NE.0)GO TO 10
*       OPEN(1,FILE=FL(1:N),STATUS='OLD',ERR=10)
#endif
       CALL CSSINP(FL,'Output file:',N)
#if defined(CERNLIB_VAX)
       OPEN(2,FILE=FL(1:N),STATUS='UNKNOWN',ERR=10)
#endif
#if defined(CERNLIB_APOLLO)
       OPEN(2,FILE=FL(1:N),STATUS='UNKNOWN',ERR=10)
#endif
#if defined(CERNLIB_IBM)
        VM=VM2//FL(1:N)//' ( RECFM F LRECL 80'
        CALL VMCMS(VM,IERR)
        IF(IERR.NE.0)GO TO 10
*       OPEN(2,FILE=FL(1:N),STATUS='UNKNOWN',ERR=10)
#endif
       CALL PROCED(SUB,NST,NS,COM,NCT,NC)
       CALL CREATE(SUB,NST,NS,COM,NCT,NC)
       PRINT 1,NC,NS
  1   FORMAT(1X,'CST -- ',I4,' COMMONS',4X,I4,' ROUTINES')
      STOP
  10  PRINT 11,FL(1:N)
  11  FORMAT(1X,'CST -- CAN''T OPEN ',A)
      END
