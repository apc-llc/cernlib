*
* $Id: btest1.F,v 1.4 1997/08/29 13:34:01 mclareni Exp $
*
* $Log: btest1.F,v $
* Revision 1.4  1997/08/29 13:34:01  mclareni
* Add new test, zebrz3 to test RZ exchange mode and CIO
*
* Revision 1.3  1997/07/04 16:54:16  mclareni
* Change the Linux BACKSLASH to agree with the gcc default
*
* Revision 1.2  1996/09/23 12:27:40  cernlib
* add Linux
*
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
#if defined(CERNLIB_QMCDC)
      PROGRAM BTEST1(INPUT,OUTPUT,TAPE1)
#endif
#if !defined(CERNLIB_QMCDC)
      PROGRAM BTEST1
#endif
*
********************************************************************************
*                                                                              *
*        Batch test program number 1 for RZ                                    *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 16.04.86                                                          *
*  Last mod: 07.05.86                                                          *
*                                                                              *
********************************************************************************
*
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(50000)
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
      DIMENSION KEY(10)
      CHARACTER*8 CHTAG(10),RTIME
#if defined(CERNLIB_TESTCX)
      CHARACTER*16 CHDIR
#endif
      CHARACTER*1 BSLSH
#if defined(CERNLIB_QMIBM)||defined(CERNLIB_QMCRY) \
  ||defined(CERNLIB_QMNXT)||defined(CERNLIB_QMCV32)
      PARAMETER (BSLSH='\')
#elif defined(CERNLIB_QMIRT)||defined(CERNLIB_QMLNX)
      PARAMETER (BSLSH='\\')
#elif 1 
      PARAMETER (BSLSH=CHAR(92))
#endif
      DIMENSION IA(10000)
      DATA IA/10000*0/
*
*     -----------------------------------------------------------------
*
#if !defined(CERNLIB_TESTCX)

#if defined(CERNLIB_QMALT)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMMPW)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIBX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT)
      OPEN(UNIT=1,FILE='rzcomp.dat',ACCESS='DIRECT',RECL=6000,
     +      STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCV64)
      OPEN(UNIT=1,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=12000)
#endif
#if defined(CERNLIB_QMVAX)||defined(CERNLIB_QMVMI)||defined(CERNLIB_QMSGI)
      OPEN(UNIT=1,FILE='RZCOMP.DAT',ACCESS='DIRECT',RECL=1500,
     +      STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMIBM)
      OPEN(UNIT=1,ACCESS='DIRECT',RECL=6000,STATUS='UNKNOWN')
      CALL TIMEST(9999.)
#endif
#if defined(CERNLIB_QMCDC)
      OPEN(UNIT=1,ACCESS='DIRECT',RECL=1500,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMCV32)
      OPEN(UNIT=1,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=6000)
#endif

#endif
C
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(50000))
      CALL TIMED(T0)
      CALL ETIMEC(RTIME)
C
C            Create RZ file
C
#if defined(CERNLIB_TESTCX)
      CALL RZOPEN(LUN1,CHDIR,'RZTEST3.DAT','CNX',1500,ISTAT)
       if(ISTAT.NE.0)print*,' Error trying to create file'
      CALL RZMAKE(LUN1,'RZTEST',1,'I','TOPTAG  ',1024,'CIX')
#else
      CALL RZMAKE(1,'RZTEST',1,'I','TOPTAG  ',1024,' ')
#endif
C
C            Create 2 subdirectories
C
      CHTAG(1)='INDEX'
      CALL RZMDIR('DIR1',1,'I',CHTAG)
      CALL RZMDIR('DIR2',1,'I',CHTAG)
C
      CALL RZCDIR('DIR1',' ')
C
      CALL TIMED(T1)
      CALL ETIMEC(RTIME)
      PRINT 1000,RTIME,T1
 1000 FORMAT(' STEP 1: REAL TIME ',A,' CP TIME',F10.5,' SECONDS')
C
C            Add,Retrieve and verify some records
C
      DO 10 J=1,100
         NWORDS=J
         IA(NWORDS)=NWORDS
         CALL RZVOUT(IA,NWORDS,J,ICYCLE,' ')
         CALL RZVIN (IA,10000,N,J,ICYCLE,' ')
         IF(IA(NWORDS).NE.NWORDS)GO TO 90
  10  CONTINUE
C
      CALL TIMED(T2)
      CALL ETIMEC(RTIME)
      PRINT 2000,RTIME,T2
 2000 FORMAT(' STEP 2: REAL TIME ',A,' CP TIME',F10.5,' SECONDS')
C
      CALL RZCDIR(BSLSH//'DIR2',' ')
C
      DO 20 J=1,100
         NWORDS=J*J
         IA(NWORDS)=NWORDS
         CALL RZVOUT(IA,NWORDS,J,ICYCLE,' ')
         CALL RZVIN (IA,10000,N,J,ICYCLE,' ')
         IF(IA(NWORDS).NE.NWORDS)GO TO 90
  20  CONTINUE
C
      CALL TIMED(T3)
      CALL ETIMEC(RTIME)
      PRINT 3000,RTIME,T3
 3000 FORMAT(' STEP 3: REAL TIME ',A,' CP TIME',F10.5,' SECONDS')
C
C             List directories and then delete all objects
C
      CALL RZCDIR(BSLSH,' ')
      CALL RZLDIR('DIR1',' ')
      CALL RZLDIR('DIR2',' ')
C
      CALL TIMED(T3)
      CALL RZDELT('DIR1')
      CALL RZDELT('DIR2')
C
      CALL TIMED(T4)
      CALL ETIMEC(RTIME)
      PRINT 4000,RTIME,T4
 4000 FORMAT(' STEP 4: REAL TIME ',A,' CP TIME',F10.5,' SECONDS')
C
      CALL RZEND('RZTEST')
C
      STOP
C
  90  PRINT 9000,J,NWORDS
 9000 FORMAT(' ERROR  WHEN J=',I3, '   NWORDS =',I6)
      STOP
      END
      SUBROUTINE ETIMEC(ELAPSD)
C***********************************************************************
C                                                                      *
C     ROUTINE FUNCTIONALLY EQUIVALENT TO TIMED, BUT FOR REAL TIME      *
C                                                                      *
C***********************************************************************
C
      CHARACTER*(8) ELAPSD
      INTEGER ND(2),NT(2)
      CHARACTER*(8) CD,CT
      CHARACTER*(*) DIGIT
      PARAMETER (DIGIT='0123456789')
      INTEGER IOLDAY,IOLSEC,MONTH(0:12)
      DATA IOLDAY,IOLSEC /0,0/
      DATA MONTH /0,31,59,90,120,151,181,212,243,273,304,334,365/
C
      CALL DATIMH(ND,NT)
      CALL UHTOC(ND,100,CD,8)
      CALL UHTOC(NT,100,CT,8)
C
C     CONVERT DATE INTO NUMBER OF DAYS FROM START OF YEAR
C
      NHIGH=INDEX(DIGIT,CD(1:1))-1
      NLOW=INDEX(DIGIT,CD(2:2))-1
      NDAY=NHIGH*10+NLOW
C
      NHIGH=INDEX(DIGIT,CD(4:4))-1
      NLOW=INDEX(DIGIT,CD(5:5))-1
      NMONTH=NHIGH*10+NLOW
C
      NHIGH=INDEX(DIGIT,CD(7:7))-1
      NLOW=INDEX(DIGIT,CD(8:8))-1
      NYEAR=NHIGH*10+NLOW
C
      IDAY=NDAY-1+MONTH(NMONTH-1)
      IF ((NYEAR/4)*4.EQ.NYEAR.AND.NMONTH.GT.2) IDAY=IDAY+1
C
C     CONVERT TIME INTO NUMBER OF SECONDS FROM START OF DAY
C
      NHIGH=INDEX(DIGIT,CT(1:1))-1
      NLOW=INDEX(DIGIT,CT(2:2))-1
      NHOUR=NHIGH*10+NLOW
C
      NHIGH=INDEX(DIGIT,CT(4:4))-1
      NLOW=INDEX(DIGIT,CT(5:5))-1
      NMINUT=NHIGH*10+NLOW
C
      NHIGH=INDEX(DIGIT,CT(7:7))-1
      NLOW=INDEX(DIGIT,CT(8:8))-1
      NSECON=NHIGH*10+NLOW
C
      ISEC=NSECON+NMINUT*60+NHOUR*3600
C
C     COMPUTE THE ELAPSED TIME FROM PREVIOUS CALL
C        (IN HH:MM:SS FORMAT)
C
      IDELAP=IDAY-IOLDAY
      ISELAP=ISEC-IOLSEC
      IOLSEC=ISEC
      IOLDAY=IDAY
      ISEC=MOD(ISELAP,60)
      IMINUT=(MOD(ISELAP,3600)-ISEC)/60
      IHOUR=ISELAP/3600+IDELAP*24
      IF (IHOUR.GT.99) THEN
         WRITE(ELAPSD,1100) IMINUT,ISEC
      ELSE
         WRITE(ELAPSD,1000) IHOUR,IMINUT,ISEC
      ENDIF
      RETURN
1000  FORMAT(I2.2,':',I2.2,':',I2.2)
1100  FORMAT('**:',I2.2,':',I2.2)
      END
      SUBROUTINE QNEXT
      WRITE(*,*) ' DUMMY QNEXT REACHED'
      END
