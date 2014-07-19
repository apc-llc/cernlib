*
* $Id: btest2.F,v 1.4 1997/09/02 15:16:16 mclareni Exp $
*
* $Log: btest2.F,v $
* Revision 1.4  1997/09/02 15:16:16  mclareni
* WINNT corrections
*
* Revision 1.3  1997/07/04 16:54:18  mclareni
* Change the Linux BACKSLASH to agree with the gcc default
*
* Revision 1.2  1996/09/23 12:27:43  cernlib
* add Linux
*
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      PROGRAM BTEST2
*
********************************************************************************
*                                                                              *
*        Batch test program number 2 for RZ                                    *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 05.04.86                                                          *
*  Last mod: 07.05.86                                                          *
*                                                                              *
********************************************************************************
*
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(30000)
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
      DIMENSION KEY(10)
      CHARACTER*1 BSLSH
#if defined(CERNLIB_QMIBM)||defined(CERNLIB_QMCRY) \
  ||defined(CERNLIB_QMNXT)||defined(CERNLIB_QMCV32)
      PARAMETER (BSLSH='\')
#elif defined(CERNLIB_QMIRT)||defined(CERNLIB_QMLNX)
      PARAMETER (BSLSH='\\')
#elif 1
      PARAMETER (BSLSH=CHAR(92))
#endif
      CHARACTER*8 CHTAG(10)
*
*     ---------------------------------------------------------------------
*
#if defined(CERNLIB_QMALT)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMMPW)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIBX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT)||defined(CERNLIB_WINNT)
      OPEN(UNIT=1,FILE='rztest.dat',ACCESS='DIRECT',RECL=4096,
     +      STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCV64)
      OPEN(UNIT=1,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=8192)
#endif
#if defined(CERNLIB_QMVAX)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMVMI)
      OPEN(UNIT=1,FILE='RZTEST.DAT',ACCESS='DIRECT',RECL=1024,
     +      STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMIBM)
      OPEN(UNIT=1,ACCESS='DIRECT',RECL=4096,STATUS='UNKNOWN')
      CALL TIMEST(9999.)
#endif
#if defined(CERNLIB_QMCDC)
      OPEN(UNIT=1,ACCESS='DIRECT',RECL=1024,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_QMCV32)
      OPEN(UNIT=1,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=4096)
#endif
C
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(30000))
      CALL TIMED(T0)
C
C            Create RZ file
C
      CALL RZMAKE(1,'RZTEST',1,'I','TOPTAG  ',1000,' ')
C
C            Create 2 subdirectories
C
      CHTAG(1)='VOLUME'
      CHTAG(2)='NUMBER'
      CALL RZMDIR('CDET',1,'I',CHTAG)
      CALL RZMDIR('ECAP',2,'II',CHTAG)
C
      CALL RZLDIR(' ',' ')
      CALL RZCDIR('CDET',' ')
C
      CALL TIMED(T1)
      PRINT 1000,T1
 1000 FORMAT(' TIME FOR STEP 1= ',F10.5,' SECONDS')
C
C            Create objects in directory /CDET
C
      NWW=0
      DO 10 I=1,50
         J=5*RNDM(I)
         NWORDS=5000*RNDM(J)
         CALL MZBOOK(IXSTOR,LBANK,LEV,1,'LEV ',0,0,NWORDS,2,0)
         KEY(1)=J
         CALL RZOUT(IXSTOR,LEV,KEY,ICYCLE,'W')
         NWW=NWW+NWORDS
  10  CONTINUE
C
      CALL RZLDIR('//RZTEST/CDET',' ')
C
      CALL RZPURG(1)
C
      CALL RZLDIR(' ',' ')
      PRINT 1500,NWW
 1500 FORMAT(' Number of data words written in /CDET ',I7,/)
C
      CALL TIMED(T2)
      PRINT 2000,T2
 2000 FORMAT(' TIME FOR STEP 2= ',F10.5,' SECONDS')
C
      CALL RZCDIR(BSLSH//'ECAP',' ')
C
C            Create objects in directory /ECAP
C
      NWW=0
      DO 20 I=1,100
         KEY(1)=I
         KEY(2)=1
         IF(I.GT.50)KEY(2)=2
         CALL MZBOOK(IXSTOR,LBANK,LEV,1,'LEV ',0,0,I,2,0)
         CALL RZOUT(IXSTOR,LBANK,KEY,ICYCLE,'W')
         NWW=NWW+I
  20  CONTINUE
C
      CALL RZLDIR(' ',' ')
      PRINT 2500,NWW
 2500 FORMAT(' Number of data words written in /ECAP ',I7,/)
C
      CALL TIMED(T3)
      PRINT 3000,T3
 3000 FORMAT(' TIME FOR STEP 3= ',F10.5,' SECONDS')
C
C             Read randomly objects from directory /ECAP
C
      NWR=0
      DO 30 I=1,500
         KEY(1)=100*RNDM(I)+1
         KEY(2)=1
         IF(KEY(1).GT.50)KEY(2)=2
         CALL RZIN(IXSTOR,LEV,1,KEY,99,' ')
         IF(LEV.GT.0)NWR=NWR+IQ(LEV-1)
         CALL MZDROP(IXSTOR,LEV,' ')
  30  CONTINUE
      PRINT 3500,NWR
 3500 FORMAT(' Number of data words read from /ECAP ',I7,/)
C
      CALL TIMED(T4)
      PRINT 4000,T4
 4000 FORMAT(' TIME FOR STEP 4= ',F10.5,' SECONDS')
C
      CALL RZEND('RZTEST')
C
      STOP
      END
      SUBROUTINE QNEXT
      WRITE(*,*) ' DUMMY QNEXT REACHED'
      END
