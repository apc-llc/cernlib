*
* $Id: uglast.F,v 1.4 1996/09/30 14:24:47 ravndal Exp $
*
* $Log: uglast.F,v $
* Revision 1.4  1996/09/30 14:24:47  ravndal
* Windows NT related modifications
*
* Revision 1.3  1996/03/15 17:23:03  japost
* Addition of parallel code
*
* Revision 1.2  1996/02/01 14:37:29  ravndal
* Clean up of the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 12/01/95  17.38.44  by  S.Ravndal
*-- Author :
      SUBROUTINE UGLAST
C
C Termination routine to print histograms and statistics
C
#include "geant321/gcflag.inc"
#include "pvolum.inc"
#include "celoss.inc"
      COMMON/SCLAST/XSEL1(40),XSEL2(40),XSEL1C(40),XSEL2C(40)
     +             ,XSER1(40),XSER2(40),XSER1C(40),XSER2C(40)
 
      DIMENSION PAT1(50),PAT2(50)
      CHARACTER*(*) FILNAM, FSTAT
      INTEGER IDVEC(100),IDN
      INTEGER ENTR
      REAL MEAN,RMS
      REAL DEF(100,2)
C
C     Control values, which have been obtained
C     using:
C     HP 720 running HP-UX 9
C     GEANT 3.21/02, CERNLIB release 94A
C
      DATA ((DEF(I,J),J=1,2),I=1,8)
     +        /94.20  ,1.0208,
     +         10.1478,3.8322,
     +          4.1152,3.8059,
     +         14.6771,4.1214,
     +         11.8910,5.4774,
     +         11.4431,3.8798,
     +         10.0794,3.6276,
     +         10.3324,3.6949/
C
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)
      PARAMETER (FILNAM='ginst1.dat')
#endif
#if defined(CERNLIB_IBM)
      PARAMETER (FILNAM='/GINST1 DAT *')
#endif
*
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      PARAMETER (FSTAT='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
      PARAMETER (FSTAT='UNKNOWN')
#endif
C
      CALL GLAST
C
C Normalize and print energy distribution
C
      CALL VZERO(XSEL1,320)
      CALL VZERO(PAT1,50)
      CALL VZERO(PAT2,50)
C
      IF (EINTOT.NE.0.0) THEN
          CNORM  = 100./EINTOT
      ELSE
          CNORM = 0.0
      END IF
C
      XEVENT=IEVENT
C
      DO 10 I = 1,NL
      XSEL1 (I) = CNORM * SEL1 (I)
      XSEL2 (I) = CNORM*SQRT(ABS(XEVENT*SEL2 (I) - SEL1 (I)**2))
 
      XSEL1C(I) = CNORM * SEL1C(I)
      XSEL2C(I) = CNORM*SQRT(ABS(XEVENT*SEL2C(I) - SEL1C(I)**2))
   10 CONTINUE
 
      CALL HPAK (2,XSEL1 )
      CALL HPAKE(2,XSEL2 )
 
      CALL HPAK (4,XSEL1C)
      CALL HPAKE(4,XSEL2C)
 
      DO 20 I = 1,NR
      XSER1 (I) = CNORM * SER1 (I)
      XSER2 (I) = CNORM*SQRT(ABS(XEVENT*SER2 (I) - SER1 (I)**2))
 
      XSER1C(I) = CNORM * SER1C(I)
      XSER2C(I) = CNORM*SQRT(ABS(XEVENT*SER2C(I) - SER1C(I)**2))
   20 CONTINUE
 
      CALL HPAK (3,XSER1 )
      CALL HPAKE(3,XSER2 )
 
      CALL HPAK (5,XSER1C)
      CALL HPAKE(5,XSER2C)
 
      DO 40 IP = 1,3
      DO 30 I  = 1,NL
          IF (XEVENT .NE. 0.0) THEN
              PAT1(I) = SNPAT1(I,IP) / XEVENT
              PAT2(I) = SQRT(SNPAT2(I,IP)/XEVENT - PAT1(I)**2)
          ELSE
              PAT1(I) = 0.0
              PAT2(I) = 0.0
          END IF
   30 CONTINUE
      CALL HPAK (10+IP,PAT1)
      CALL HPAKE(10+IP,PAT2)
   40 CONTINUE
C
C     Create a output file on unit 99 for the default installation
C     test run
C     The test consists of 10 events of 10 GeV photons
C
      OPEN(UNIT=99,FILE=FILNAM,STATUS=FSTAT,FORM='FORMATTED')
C
      WRITE(99,15000)
      WRITE(99,15001)
      WRITE(99,15002)
      WRITE(99,15003)
      WRITE(99,15004)
      WRITE(99,15005)
      WRITE(99,15006)
C
      CALL HID1(IDVEC,IDN)
      IF(IDN.GE.1.AND.IDN.LE.100) THEN
C
          CALL HNOENT(1,ENTR)
          WRITE(99,20000) ENTR
          DO I=1,IDN
              MEAN = HSTATI(IDVEC(I),1,' ',0)
              RMS  = HSTATI(IDVEC(I),2,' ',0)
              WRITE(99,20100) IDVEC(I)
              WRITE(99,20200) MEAN,RMS
              WRITE(99,20300) DEF(I,1),DEF(I,2)
          END DO
      END IF
C
      CLOSE(99)
C
      PRINT 10000
      PRINT 10200,( XSEL2 (I),I=1,NL)
      PRINT 10300,( XSEL2C(I),I=1,NL)
      PRINT 10100
      PRINT 10200,( XSER2 (I),I=1,NR)
      PRINT 10300,( XSER2C(I),I=1,NR)
10000 FORMAT(/,1x,'LONGITUDINAL PROFIL',/)
10100 FORMAT(/,1x,'   RADIAL PROFIL   ',/)
10200 FORMAT(/,1X,'ERROR ON PROFIL     VALUES',/
     +         ,(5X,10F10.4))
10300 FORMAT(//,1X,'ERROR ON CUMULATIVE VALUES',/
     +         ,(5X,10F10.4))
C
15000 FORMAT(/,'                   GEANT Installation control file')
15001 FORMAT('                   _______________________________')
15002 FORMAT('The default values were obtained using:')
15003 FORMAT(/,3X,'GEANT Version 3.21/02')
15004 FORMAT(3X,'CERNLIB release 94A')
15005 FORMAT(3X,'HP 720 running HP-UX 9')
15006 FORMAT(3X,'Date: 11 Oct 1994')
20000 FORMAT(/,1x,'Number of Entries ',I6,20X,'Mean',6X,'RMS')
20100 FORMAT(/,1x,'Histogram Id: ',I3)
20200 FORMAT(1X,'Mean Entry value/R.M.S           :'
     +         ,(5X,2F10.4))
20300 FORMAT(1X,'Default values of GEANT 3.21/02  :'
     +         ,(5X,2F10.4))

#if  !defined(CERNLIB_PARA)
C
C Save histograms to file
C
      CALL HRPUT(0,'gexam1.hbook',' ')
C
      CALL HISTDO()
#endif
#if   defined(CERNLIB_PARA)
C     Save histograms
C
c     Gpsumhr is a replacement for hrput that saves histograms into a file,
c      summing the contribution from each node in the subdirectory 'totals', 
c      and also storing each node's contribution separately elsewhere.
c     It has the same arguments as hrput: (histogramid, filename, options).
c
      call gpsumhr(0,'gexam3.hist',' ')
      call gprocs( nsize, nrank, nleader )
      if ( nrank .eq. nsize-1 ) then
          CALL HISTDO
      endif

c     The call to gplast will TERMINATE parallel GEANT. 
c     It cleans up the parallel environment.
c     The program will not return from this call.

      call gplast
#endif
C
      END
