*
* $Id: uglast.F,v 1.2 1996/02/22 13:24:06 ravndal Exp $
*
* $Log: uglast.F,v $
* Revision 1.2  1996/02/22 13:24:06  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:22  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGLAST
C.    *
C.    *      Termination routine to print histograms and statistics
C.    *
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "pvolum.inc"
#include "celoss.inc"
#include "bootwk.inc"
C
      DIMENSION     XSEL1(NBIN),XSEL1C(NBIN),XSER1(NBIN),XSER1C(NBIN),
     +              XSEL2(NBIN),XSEL2C(NBIN),XSER2(NBIN),XSER2C(NBIN),
     +               PAT1(NBIN), PAT2(NBIN)
 
      IF(ISWIT(10).EQ.0)CALL GLAST
 
C ***          Normalize and print energy distribution
 
      XEVENT=IEVENT
      CNORM  = 100./(XEVENT*PKINE(3))
C
      DO 2 I = 1,NLTOT
         XSEL1 (I) = CNORM * SEL1 (I)
         XSEL2 (I) = CNORM*SQRT(ABS(XEVENT*SEL2 (I) - SEL1 (I)**2))
         XSEL1C(I) = CNORM * SEL1C(I)
         XSEL2C(I) = CNORM*SQRT(ABS(XEVENT*SEL2C(I) - SEL1C(I)**2))
    2 CONTINUE
 
      CALL HPAK (2,XSEL1 )
      CALL HPAKE(2,XSEL2 )
 
      CALL HPAK (4,XSEL1C)
      CALL HPAKE(4,XSEL2C)
 
      DO 3 I = 1,NRTOT
         XSER1 (I) = CNORM * SER1 (I)
         XSER2 (I) = CNORM*SQRT(ABS(XEVENT*SER2 (I) - SER1 (I)**2))
         XSER1C(I) = CNORM * SER1C(I)
         XSER2C(I) = CNORM*SQRT(ABS(XEVENT*SER2C(I) - SER1C(I)**2))
    3 CONTINUE
 
      CALL HPAK (3,XSER1 )
      CALL HPAKE(3,XSER2 )
 
      CALL HPAK (5,XSER1C)
      CALL HPAKE(5,XSER2C)
 
      IPMX=3
      IF(IKINE.GT.3) IPMX=4
      DO 14 IP = 1,IPMX
      DO 13 I  = 1,NLTOT
         PAT1(I) = SNPAT1(I,IP) / XEVENT
         PAT2(I) = SQRT(SNPAT2(I,IP)/XEVENT - PAT1(I)**2)
   13 CONTINUE
      CALL HPAK (10+IP,PAT1)
      CALL HPAKE(10+IP,PAT2)
   14 CONTINUE
*
* *** NORMALISE HISTO
*
      IF(JBOOT.NE.0) CALL HNORMA(1000, HSUM(1000)/XEVENT)
 
      PRINT 751
      PRINT 754 ,( XSEL2 (I),I=1,NLTOT)
      PRINT 755 ,( XSEL2C(I),I=1,NLTOT)
      PRINT 752
      PRINT 754 ,( XSER2 (I),I=1,NRTOT)
      PRINT 755 ,( XSER2C(I),I=1,NRTOT)
  751 FORMAT(///,40X,'LONGITUDINAL PROFIL',/)
  752 FORMAT(///,40X,'   RADIAL PROFIL   ',/)
  754 FORMAT(//,30X,'ERROR ON PROFIL     VALUES',/
     *         ,(10X,10F10.4))
  755 FORMAT(//,30X,'ERROR ON CUMULATIVE VALUES',/
     *         ,(10X,10F10.4))
C
C             Save histograms
C
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('HBOOK')
C
      IF(ISWIT(10).EQ.0)THEN
         CALL HIDOPT(0,'BLAC')
         CALL HISTDO
      ENDIF
      END
