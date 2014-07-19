*
* $Id: main.F,v 1.1.1.1 1996/03/06 10:47:04 mclareni Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:04  mclareni
* Zebra
*
*
#if defined(CERNLIB_INTER)
#include "test_include/pilot.h"
      PROGRAM TEST
*
********************************************************************************
*                                                                              *
*         Interactive test program for RZ, based on ZCEDEX                     *
*         Runs on Apollo,VAX and VM                                            *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 02.04.86                                                          *
*  Last mod: 12.04.86                                                          *
*                                                                              *
********************************************************************************
*
#if defined(CERNLIB_QMAPO)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(500000)
#endif
#if defined(CERNLIB_QMVAX)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(500000)
#endif
#if (!defined(CERNLIB_QMAPO))&&(!defined(CERNLIB_QMVAX))
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(100000)
#endif
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
      COMMON/ZCEDEX/ZZZZ(15000)
      COMMON /ZCTCOM/ IHANDL,ICLNUP(2),ICTPAR,BREAK
      COMMON/RZUSER/NVIN,NVOUT,IV(10000)
*
*-----------------------------------------------------------------------
*
#if defined(CERNLIB_QMALT)
      OPEN(UNIT=11,FILE='../LIBZC',ACCESS='DIRECT')
      OPEN(UNIT=1,FILE='RZTEST.DAT' ,ACCESS='DIRECT',RECL=4096)
      OPEN(UNIT=2,FILE='RZTEST2.DAT',ACCESS='DIRECT',RECL=2048)
      OPEN(UNIT=3,FILE='RZFZ.DAT',FORM='UNFORMATTED',RECL=10000)
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(500000))
#endif
#if defined(CERNLIB_QMAPO)
%INCLUDE '/SYS/INS/PFM.INS.FTN'
      OPEN(UNIT=11,FILE='\LIBZC',ACCESS='DIRECT')
      OPEN(UNIT=1,FILE='RZTEST.DAT' ,ACCESS='DIRECT',RECL=4096)
      OPEN(UNIT=2,FILE='RZTEST2.DAT',ACCESS='DIRECT',RECL=2048)
      OPEN(UNIT=3,FILE='RZFZ.DAT',FORM='UNFORMATTED',RECL=10000)
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(500000))
#endif
#if defined(CERNLIB_QMVAX)
      OPEN(UNIT=1,FILE='RZTEST.DAT' ,ACCESS='DIRECT',RECL=1024,
     +      STATUS='UNKNOWN',SHARED,ORGANIZATION='RELATIVE')
      OPEN(UNIT=2,FILE='RZTEST2.DAT',ACCESS='DIRECT',RECL=512,
     +      STATUS='UNKNOWN',SHARED,ORGANIZATION='RELATIVE')
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(500000))
#endif
#if defined(CERNLIB_QMIBM)
      OPEN(UNIT=1,ACCESS='DIRECT',RECL=4096,STATUS='UNKNOWN')
      OPEN(UNIT=2,ACCESS='DIRECT',RECL=2048,STATUS='UNKNOWN')
      CALL TIMEST(9999.)
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(100000))
#endif
C
      CALL ZCINIT(15000)
C=== THIS IS THE SECTION TO HANDLE BREAK, EFFECTIVE ON VAX & APOLLO
      CALL ZCENBR
C=== END OF BREAK SECTION
C
C             Creation of test commands
C
      CALL RZTIN
      NVIN=0
      NVOUT=100
C
C             AUTOMATIC LOGON ON FILE 11
C
      CALL ZCLOGO(11)
#if defined(CERNLIB_QMAPO)
C
C             BREAK SECTION ONLY FOR APOLLO
C
      ISTAT=PFM_$CLEANUP(ICLNUP)
C
C             RECOGNIZE FAULTS, EFFECTIVE ONLY ON APOLLO
C
      CALL ZCRECF(ISTAT)
#endif
C
C             COME BACK HERE FOR EACH NEW COMMAND
C
   1  CALL ZCGET(NMENU,NCOM,NPAR)
C
C
      IF(NMENU.EQ.1)THEN
         CALL RZT(NCOM,NPAR)
         GO TO 1
      ELSEIF(NMENU.EQ.2)THEN
         CALL DZRT(NCOM,NPAR)
         GO TO 1
      ENDIF
C
      STOP
      END
#endif
