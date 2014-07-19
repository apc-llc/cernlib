*
* $Id: uglast.F,v 1.3 1996/03/15 15:50:07 japost Exp $
*
* $Log: uglast.F,v $
* Revision 1.3  1996/03/15 15:50:07  japost
* Small corrections to Parallel code
*
* Revision 1.2  1996/03/13 17:30:33  ravndal
* Modifications for parallel version testing included
*
c Revision 1.1.1.1  95/10/24  10:22:12  cernlib
c Geant
c 
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 12/01/95  17.38.02  by  S.Ravndal
*-- Author :
      SUBROUTINE UGLAST
C
#include "geant321/gclist.inc"
      CHARACTER*(*) FILNAM, FSTAT
      INTEGER IDVEC(100),IDN
      INTEGER ENTR
      REAL MEAN,RMS
      REAL DEF(100,2)
C
C
C     Control values, which have been obtained
C     using:
C     HP 720 running HP-UX 9
C     GEANT 3.21/02, CERNLIB release 94A
C
      DATA ((DEF(I,J),J=1,2),I=1,6)
     +        / -19.3125,22.4784,
     +            2.2500, 7.1458,
     +            -.5000, 0.0000,
     +           15.7857,14.0277,
     +            1.5000, 7.4066,
     +            -.5000,  .0000/
C
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)
      PARAMETER (FILNAM='g_install3.dat')
#endif
#if defined(CERNLIB_IBM)
      PARAMETER (FILNAM='/G_INSTALL3 DAT *')
#endif
*
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      PARAMETER (FSTAT='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
      PARAMETER (FSTAT='UNKNOWN')
#endif
#if  defined(CERNLIB_PARA)
      integer  nsize, nrank, nleader 
#endif
      CALL GLAST
C
      IF(NGET.NE.0.OR.NSAVE.NE.0) CALL GCLOSE(0,IER)
C
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
C
#if !defined(CERNLIB_PARA)
      CALL HISTDO
#endif
#if  defined(CERNLIB_PARA)
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
      RETURN
15000 FORMAT(/,'                   GEANT Installation control file')
15001 FORMAT('                   _______________________________')
15002 FORMAT(/,'The default values were obtained using:')
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
      END
