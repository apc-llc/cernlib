*
* $Id: uginit.F,v 1.2 1996/02/22 13:16:33 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.2  1996/02/22 13:16:33  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
C.    *
C.    *        To initialise GEANT/USER  program and read data cards
C.    *
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcunit.inc"
#include "shobuf.inc"
*
*             Open user files
*
      CALL UFILES
C
C *** Initialise GEANT
C
      CALL GINIT
C
C *** Prints version number
C
      WRITE(LOUT,1000)
C
C *** Material
C
      NMAT= 8
C
C *** Grid (sizes must be in unit of radiation length)
C          (they will be converted in cm by the routine UGEOM)
C
      NXBIN = 40
      NYBIN = 40
      NZBIN = 40
       XBIN = 0.5
       YBIN = 0.5
       ZBIN = 1.0
C
C *** Incident particle
C
      IKINE = 1
C
C *** Shower library
C
      CALL VZERO(ESHOW,20)
      CALL VZERO(NSHOW,20)
      CALL VZERO(NFIRE,20)
      CALL VZERO(NRCOR,20)
      CUTLOW = 0.0001
C
C *** Logical unit to write shower file
C
      LUNITS(1) = 50
C
C *** Overwrite some previous initializations from data cards
C
      CALL FFKEY('MATE',NMAT    , 1,'INTEGER')
      CALL FFKEY('GRID',NXBIN   , 6,'MIXED'  )
      CALL FFKEY('ESHO',ESHOW(1),20,'REAL'   )
      CALL FFKEY('NSHO',NSHOW(1),20,'INTEGER')
C
      CALL GFFGO
C
C *** Setup data structures and banks.
C
      CALL GZINIT
C
C *** Geometry and materials description
C
      CALL UGEOM
C
C *** Particles, energy loss and cross-sections initialisations
C
      CALL GPART
      CALL GPHYSI
C
C *** Total number of events
C
      NENERG = 0
      NEVENT = 0
      DO 4 I=1,20
      NSHOW(I) = MIN(NSHOW(I),NWORD-3)
      IF(NSHOW(I).LE.0) GO TO 5
      NENERG = NENERG + 1
    4 NEVENT = NEVENT+NSHOW(I)
C
   5  IF(NENERG.LE.0) RETURN
      IENERG = 1
      ISHOW  = 0
C
C *** Shower origin (bin number; coordinates)
C
      IX = NXBIN/2 + 1
      IY = NYBIN/2 + 1
      IZ = NZBIN/4 + 1
C
      IRI = NXBIN*NYBIN*(IZ-1)+NXBIN*(IY-1)+IX+1
C
      XRI = (IX-0.5)*XBIN
      YRI = (IY-0.5)*YBIN
      ZRI = (IZ-0.5)*ZBIN
C
      XGENER = XRI-0.5*NXBIN*XBIN
      YGENER = YRI-0.5*NYBIN*YBIN
      ZGENER = ZRI-0.5*NZBIN*ZBIN
C
C *** Write out header block
C
      CALL VZERO(IBUF,NWORD)
      IBUF(1)=NENERG+1
      JPA = LQ(JPART-IKINE)
      CHARGE = Q(JPA+8)
      IBUF(2)=INT(CHARGE)
      IBUF(3)=IRI
      IBUF(4)=NXBIN
      IBUF(5)=NYBIN
      IBUF(6)=NZBIN
       BUF(7)= XBIN
       BUF(8)= YBIN
       BUF(9)= ZBIN
      IBUF(10)= NENERG
       BUF(11)= CUTLOW*1000.
      IBUF(12)= NXBIN*NYBIN
       BUF(13)= XRI
       BUF(14)= YRI
       BUF(15)= ZRI
C
#if defined(CERNLIB_CRAY)
      OPEN(LUNITS(1),FILE='bootgen1.dat',ACCESS='DIRECT',
     +     FORM='UNFORMATTED',RECL=2*NBYTE,STATUS='NEW')
#endif
#if defined(CERNLIB_IBM)
      OPEN(LUNITS(1),ACCESS='DIRECT',RECL=NBYTE)
#endif
#if defined(CERNLIB_APOLLO)
      OPEN(LUNITS(1),FILE='bootgen1.dat',ACCESS='DIRECT',
     +     FORM='UNFORMATTED',RECL=NBYTE,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)
      OPEN(LUNITS(1),FILE='BOOTGEN1.DAT',ACCESS='DIRECT',
     +     FORM='UNFORMATTED',RECL=NWORD,STATUS='UNKNOWN')
#endif
      WRITE(LUNITS(1),REC=1) IBUF
      IRECOR=NENERG+2
C
CCCC  PRINT *,' FIRST  ',IBUF
C
      CALL VZERO(IBUF,NWORD)
C
C *** Define user histograms
C
      CALL UHINIT
C
 1000 FORMAT(/,'  SHOWER LIBRARY  GENERATION   VERSION  2.01 ',/)
C
      END
