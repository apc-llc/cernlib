*
* $Id: uglast.F,v 1.2 1996/02/01 14:54:52 ravndal Exp $
*
* $Log: uglast.F,v $
* Revision 1.2  1996/02/01 14:54:52  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/04 12/01/95  17.39.08  by  S.Ravndal
*-- Author :
      SUBROUTINE UGLAST
*
************************************************************************
*                                                                      *
*            Termination routine to print histograms and statistics    *
*                                                                      *
************************************************************************
*
#include "geant321/gcflag.inc"
#include "cdhscom.inc"
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
      DATA ((DEF(I,J),J=1,2),I=1,4)
     +        /1242.4821,76.8034,
     +            5.4546, 2.2776,
     +            4.1940, 2.2776,
     +             .2116,  .3326/
C
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)
      PARAMETER (FILNAM='g_install2.dat')
#endif
#if defined(CERNLIB_IBM)
      PARAMETER (FILNAM='/G_INSTALL2 DAT *')
#endif
*
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      PARAMETER (FSTAT='UNKNOWN')
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
      PARAMETER (FSTAT='UNKNOWN')
#endif
*
*     -----------------------------------------------------------------
*
      CALL GLAST
*
*             Print HBOOK histograms
*
      XNORM=1./(FLOAT(IEVENT)+0.0000001)
      CALL HOPERA(102,'+   ',102,102,XNORM,0.)
      CALL HOPERA(103,'+   ',103,103,XNORM,0.)
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
C             Save histograms
C
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('HBOOK')
      CALL HPRINT(0)
#if defined(CERNLIB_HIGZ)
*
*             Close GKS display file
*
      CALL IGEND
#endif
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
*
      END
