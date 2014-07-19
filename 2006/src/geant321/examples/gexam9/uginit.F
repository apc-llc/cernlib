*
* $Id: uginit.F,v 1.2 1996/02/22 13:32:44 ravndal Exp $
*
* $Log: uginit.F,v $
* Revision 1.2  1996/02/22 13:32:44  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:25  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
*
************************************************************************
*                                                                      *
*          To initialise GEANT3 program and read data cards            *
*                                                                      *
************************************************************************
*
#include "geant321/gclist.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
#include "geant321/gconsp.inc"
#include "urgeom.inc"
#include "tstcom.inc"
#include "geant321/gckine.inc"
#include "maphc.inc"
#include "hboot.inc"
*
*             Initialize global variables
      DIMENSION MRUN1(5)
      DATA MRUN1 /4HRUN1, 1, 1, 20, 3/, NZERO/0/
      DATA INIT/4HINIT/
*
      CALL UFILES
*
*             Initialize Geant.
      CALL GINIT
*
*             Use data cards definition
*     gexam9 cards *
      BSPOT  = 1.0
      XY0(1) = 0.0
      XY0(2) = 0.0
      XY0(3) = -66.85
      ANGL(1) = 0.0
      ANGL(2) = 0.0
*     gexam10 cards *
      PPART  = 2.0
      SIGBX  = 1.
      SIGBY  = 1.
      SIGBZ  = 0.841
      KPART  = 3
      XY0(1) = 0.0
      XY0(2) = 0.0
      XY0(3) = 11.07
      CALL VZERO(ESHOW,20)
      CALL VZERO(NSHOW,20)
*
 
      CALL FFKEY('MOME', PPART , 1, 'REAL')
      CALL FFKEY('PART', KPART , 1, 'INTE')
      CALL FFKEY('ANGL', ANGL  , 2, 'REAL')
      CALL FFKEY('XYZ0', XY0 , 3, 'REAL')
      CALL FFKEY('SIGB', SIGBX , 3, 'REAL')
      CALL FFKEY('HBOT',LHBOOT,2,'INTEGER')
      CALL FFKEY('ESHO',ESHOW(1),20,'REAL'   )
      CALL FFKEY('NSHO',NSHOW(1),20,'INTEGER')
 
      CALL GFFGO
 
      ANGL(1) = ANGL(1)*DEGRAD
      ANGL(2) = ANGL(2)*DEGRAD
*
      CALL GZINIT
*
*             Geometry and materials description.
      CALL UGEOM
*
*             Particle table definition and energy loss initialization.
      CALL GPART
      CALL GPHYSI
*
      IF(LHBOOT(1).EQ.1) THEN
*             Open I/O buffers
         IF(NGET .GT.0)CALL GOPEN(1,'I',0,IER)
         IF(NSAVE.GT.0)CALL GOPEN(2,'O',0,IER)
*
*             Prints version number
         WRITE(LOUT,10000)
         IGAST = 1
         IDSTW = 0
*
         CALL HBOOTS
*
      ELSE
*             Define "RUN1" bank to be saved
         JRUN1 = LQ(JRUNG-1)
         IF (JRUN1.NE.0) CALL MZDROP (IXSTOR, JRUN1, '    ')
         CALL MZLIFT (IXSTOR, JRUN1, JRUNG, -1, MRUN1, NZERO)
         CALL UCOPY (PPART, Q(JRUN1+1), 7)
         Q(JRUN1+8) = KPART
*
*             Open I/O buffers
         IF(NSAVE.GT.0)CALL GOPEN(2,'O',0,IER)
*
*             Prints version number
         WRITE(LOUT,10100)
*
         NENERG = 0
         NEVENT = 0
         NP1 = 0
         KEVN = 0
         DO 10 I=1,20
            IF(NSHOW(I).LE.0) GO TO 20
            NENERG = NENERG + 1
   10    NEVENT = NEVENT+NSHOW(I)*10
*
   20    IF(NENERG.LE.0) RETURN
         IENERG = 1
         ISHOW = 0
         ITHETA = 0
         IF (KPART.EQ.8 .OR. KPART.EQ.9) THEN
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)||defined(CERNLIB_CRAY)
            OPEN (UNIT=21, FILE='efrp.dat', STATUS='UNKNOWN', FORM=
#endif
#if defined(CERNLIB_IBM)
            OPEN (UNIT=21,FILE='/EFRP DAT *',STATUS='UNKNOWN',FORM=
#endif
     +      'UNFORMATTED')
         ELSE
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)||defined(CERNLIB_CRAY)
            OPEN (UNIT=21, FILE='efr.dat', STATUS='UNKNOWN', FORM=
#endif
#if defined(CERNLIB_IBM)
            OPEN (UNIT=21,FILE='/EFR DAT *',STATUS='UNKNOWN',FORM=
#endif
     +      'UNFORMATTED')
         END IF
*
         WRITE(21) NENERG,(ESHOW(I),I=1,NENERG)
*
         CALL VZERO(FMAP,17400)
         CALL VZERO(ZMAP,13500)
         CALL VZERO(RNON,10)
*
      END IF
 
      CALL UHINIT
*
10000 FORMAT(/,'**** GEXAMPLE 9: Bootstrap in L3 HCAL (one module)',
     +          '(June 1987)',/)
10100 FORMAT(/,'**** GEXAMPLE 10:  L3  HCAL (one module)
     +          makes frozen files for bootstrap ',/)
*
      END
