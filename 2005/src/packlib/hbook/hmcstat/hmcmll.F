*
* $Id: hmcmll.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hmcmll.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          18/07/95  11.45.16  by  Julian Bunn
*-- Author :    Roger Barlow, Christine Beeston   24/09/93
 
       SUBROUTINE HMCMLL(IDDATA,IDMC,IDWT,NSRC,CHOPT,
     +                   IFIXMC,FIXFRA,FLIM,START,STEP,UP,
     +                   ANSMIG,DANSMIG)
 
* Fits the given Monte Carlo distributions to the data distribution, using
* a binned maximum likelihood fit which includes the effect of both data and
* Monte Carlo statistics, and allows weights to be
* provided for each Monte Carlo distribution.  The data and Monte Carlo
* distributions must be presented in 1 dimensional histograms.
* The best estimate of the fraction of each Monte
* Carlo distribution present in the data distribution is returned, with an
* error estimate where required.
 
* Input parameters:
* -----------------
* IDDATA Data histogram identifier.
* IDMC Array of dimension NMCSRC containing Monte Carlo histogram
* identifiers.
* IDWT Array of dimension NMCSRC containing weight histogram
* identifiers. ('W' option only).
* NMCSRC Number of Monte Carlo sources.
* CHOPT - character options as follows  1 2 3 4 5 6 7
*                                       F L W S N P E
* 'F' Fix one or more of the fractions.
* 'L' Set limits on the fractions as given in FLIM.
* 'W' Use the weight histograms provided.  For non existent weight
* histograms, and if the W option is not requested, a dummy weight histogram
* in which all entries are 1 is booked.
* 'S' Scan the likelihood function with respect to each fit parameter,
* before and after the fit.  If the 'N' option is specified, the function
* will only be scanned once for each parameter.
* 'N' Do not perform the fit.
* 'P' Use the parameter start points and initial step sizes
* provided in START and STEP.  If negative values are provided the
* defaults are used.
* If the P option is
* not specified then the start point for each free parameter is
* 1.-sum of fixed fractions/nsrc-number of fixed fractions and the initial
* step size is 0.01.
* 'E' Perform a detailed error analysis using the MINUIT routines
* HESSE and MINOS.
 
* IFIXMC Array of dimension NMCSRC containing '1' if a
* parameter is to be fixed in the fit, '0' otherwise. ('F' option
* only).
* FRAC Array of dimension NMCSRC with the values at which
* parameters are to be fixed. ('F' option only).
* FLIM Array of dimension (NMCSRC,2) with the lower, then
* upper limits on the parameters. ('L' option only).
* START Array of dimension NMCSRC with the start values for
* each parameter. ('P' option only).
* STEP Array of dimension NMCSRC with initial step sizes for the
* parameters ('P' option only).
* UP 'UP' value for the error estimate ('E' option only).  See the Minuit
* manual for definition of UP.
 
* Output parameters:
* -----------------
* PAR Array of dimension NMCSRC with the final fitted values
* of the parameters.
* DPAR Array of dimension NMCSRC with the errors
* on the final fitted values of the parameters.
 
#include "hbook/hcmcpm.inc"
 
C Passed to the routine..
       INTEGER IDDATA,IDMC(*),IDWT(*),NSRC,IFIXMC(*)
       CHARACTER*(*) CHOPT
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION FLIM(2,*),START(*),STEP(*),UP,FIXFRA(*)
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL FLIM(2,*),START(*),STEP(*),UP,FIXFRA(*)
#endif
 
C Number of free MC sourceS, Sum of fixed MC fractions
       INTEGER MCFREE
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION FRACSM
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL FRACSM
#endif
       COMMON/HMCFIX/FRACSM,MCFREE
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION ANSMIG(NSRCMX),DANSMIG(NSRCMX)
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL ANSMIG(NSRCMX),DANSMIG(NSRCMX)
#endif
 
       CHARACTER*10 PNAME(NSRCMX)
 
C Stuff for Minuit
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION PSTART(NSRCMX),PSTEP(NSRCMX)
       DOUBLE PRECISION ARGLIS(10),BLO,BHI,BND1,BND2,VAL,ERROR
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL PSTART(NSRCMX),PSTEP(NSRCMX)
       REAL ARGLIS(10),BLO,BHI,BND1,BND2,VAL,ERROR
#endif
 
       LOGICAL HEXIST
 
       INTEGER JSRC,IFAIL,IVARBL,IERR
 
       EXTERNAL HFCNMIN,HEXIST
 
       DATA PNAME/'P1','P2','P3','P4','P5','P6','P7','P8','P9','P10',
     +  'P11','P12','P13','P14','P15','P16','P17','P18','P19','P20'/
       DATA DEFSTP/0.01D0/
 
C Initialisation - HMCINI checks existence and compatibility of
C Data, MC and Weight histograms.
       CALL HMCINI(IDDATA,IDMC,IDWT,NSRC,CHOPT,IERR)
       IF(IERR.EQ.1)THEN
          CALL HBUG('Returning from HMCMLL - HMCINI error',
     +    'HMCMLL',0)
          RETURN
       ENDIF
 
       CALL MNINIT(5,6,7)
 
C number of free mc sources
       MCFREE=0
       FRACSM=0.0D0
       DO 90 JSRC=1,NMCSRC
          IF(IOPT(1).EQ.0.OR.IFIXMC(JSRC).EQ.0)THEN
             MCFREE=MCFREE+1
          ELSE
             FRACSM=FRACSM+FIXFRA(JSRC)
          ENDIF
90     CONTINUE
       WRITE(6,1000)MCFREE,NMCSRC-MCFREE
1000   FORMAT(' HMCMLL: You have ',I2,' free fractions and ',
     + I2,' fixed')
 
C set up start values for parameter start points, limits, step sizes
       DO 100 JSRC =1,NMCSRC
          IF(IOPT(2).EQ.1)THEN
             BLO=FLIM(1,JSRC)
             BHI=FLIM(2,JSRC)
          ELSE
             BLO=0.0D0
             BHI=0.0D0
          ENDIF
 
          IF(IOPT(6).EQ.1)THEN
             IF(START(JSRC).LT.0.0)THEN
                WRITE(6,*)
     +' HMCMLL: Using default for start value source ',JSRC
                PSTART(JSRC)=(1.-FRACSM)/FLOAT(MCFREE)
             ELSE
                PSTART(JSRC)=START(JSRC)
             ENDIF
             IF(STEP(JSRC).LT.0.0)THEN
                WRITE(6,*)
     +' HMCMLL: Using default for step value source',JSRC
                PSTEP(JSRC)=DEFSTP
             ELSE
                PSTEP(JSRC)=STEP(JSRC)
             ENDIF
          ELSE
             PSTART(JSRC)=(1.-FRACSM)/FLOAT(MCFREE)
             PSTEP(JSRC)=DEFSTP
          ENDIF
          IF(IOPT(1).EQ.1.AND.IFIXMC(JSRC).EQ.1)THEN
             PSTART(JSRC)=FIXFRA(JSRC)
          ENDIF
 
C Initialise Minuit parameters
          CALL MNPARM(JSRC,PNAME(JSRC),PSTART(JSRC),PSTEP(JSRC),
     +    BLO,BHI,IFAIL)
          IF(IFAIL.NE.0)THEN
             CALL HBUG(
     +'Unable to define a parameter in MNPARM call','HMCMLL',0)
             STOP
          ENDIF
100    CONTINUE
 
C set title
       CALL MNSETI('*** HBOOK New ll maximisation')
 
C initialisation call to FCN
       ARGLIS(1)=1.
       CALL MNEXCM(HFCNMIN,'CALL FCN',ARGLIS,1,IFAIL,0)
 
C fix chosen fractions
       DO 110 JSRC=1,NMCSRC
          IF(IOPT(1).EQ.1.AND.IFIXMC(JSRC).EQ.1)THEN
             ARGLIS(1)=FLOAT(JSRC)
             CALL MNEXCM(HFCNMIN,'FIX',ARGLIS,1,IFAIL,0)
             WRITE(6,3000)JSRC,FIXFRA(JSRC)
          ENDIF
110    CONTINUE
3000   FORMAT(' HMCMLL: Fixed parameter number ',I2,' at ',F8.5)
 
C Set initial UP value to 0.5
       ARGLIS(1)=0.5
       CALL MNEXCM(HFCNMIN,'SET ERR',ARGLIS,1,IFAIL,0)
 
C scan log likelihood around start
       IF(IOPT(4).EQ.1)THEN
          CALL MNEXCM(HFCNMIN,'SCAN',ARGLIS,0,IFAIL,0)
       ENDIF
 
C maximise log - likelihood
       IF(IOPT(5).NE.1)THEN
          IFAIL=0
          CALL MNEXCM(HFCNMIN,'MIGRAD',ARGLIS,0,IFAIL,0)
          IF(IFAIL.NE.0)THEN
             WRITE(6,3500)IFAIL
          ENDIF
3500      FORMAT(' HMCMLL: MIGRAD ERROR FLAG ',I2)
 
C scan log likelihood around end point
          IF(IOPT(4).EQ.1)THEN
             CALL MNEXCM(HFCNMIN,'SCAN',ARGLIS,0,IFAIL,0)
          ENDIF
       ENDIF
 
C calculate minos errors
C First set UP value as appropriate for  parameters
       IF(IOPT(7).EQ.1)THEN
          IFAIL=0
          IF(UP.LE.0)THEN
             CALL HBUG(
     +'+Negative or zero UP value given - using 0.5',
     +       'HMCMLL',0)
             ARGLIS(1)=0.5
          ELSE
             ARGLIS(1)=UP
          ENDIF
          CALL MNEXCM(HFCNMIN,'SET ERR',ARGLIS,1,IFAIL,0)
          IF(IFAIL.EQ.0)THEN
             WRITE(6,4000)ARGLIS(1),MCFREE
          ENDIF
4000      FORMAT(' HMCMLL: SET UP VALUE TO ',F5.2,/,
     +           ' HMCMLL: FOR ',I2,' FREE PARAMETERS')
 
          IFAIL=0
          CALL MNEXCM(HFCNMIN,'MINOS',ARGLIS,0,IFAIL,0)
          IF(IFAIL.NE.0)THEN
             WRITE(6,5000)IFAIL
          ENDIF
5000      FORMAT(' HMCMLL: MINOS ERROR FLAG ',I2)
       ENDIF
 
C termination
       DO 130 JSRC=1,NMCSRC
          CALL MNPOUT(JSRC,PNAME(JSRC),VAL,ERROR,BND1,BND2,IVARBL)
          ANSMIG(JSRC)=VAL
          DANSMIG(JSRC)=ERROR
130    CONTINUE
 
       RETURN
       END
 
