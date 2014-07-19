*
* $Id: hmcini.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hmcini.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.05.57  by  Rene Brun
*-- Author :    Roger Barlow, Christine Beeston   24/09/93
 
      SUBROUTINE HMCINI(IDDATA,IDMC,IDWT,NSRC,CHOPT,IERR)
 
* Initialisation routine for function HMCLNL, needs to be called
* each time a new set of histograms is introduced (generally once at the
* beginning of each fit).  Performs some error checking and sets up a dummy
* weight histogram if necessary.  Normalises the weight histograms (once
* only, so the same weight histogram may be used for 2 simultaneous
* fits and calculates the overall normalisation constants bj - these are
* stored with the histograms using HNORMA.  Ids of the weight histograms
* are returned in IDWT.
 
* Input parameters:
* ----------------
* IDDATA Data histogram identifier.
* IDMC Array of dimension NMCSRC containing Monte Carlo histogram
* identifiers.
* IDWT Array of dimension NMCSRC containing weight histogram
* identifiers. ('W' option only).
* NMCSRC Number of Monte Carlo sources.
* CHOPT...
* 'W' Use the weight histograms provided.  For non existent weight
* histograms, and if the W option is not requested, a dummy weight histogram
* in which all entries are 1 is booked.
* IERR - 0 if initialisation fine, 1 if parameters sent to HMCINI were
* not useable (e.g. number of sources 0 or 1)
 
#include "hbook/hcbook.inc"
#include "hbook/hcmcpm.inc"
 
C Passed to the routine..
       INTEGER IDDATA,IDMC(*),IDWT(*),NSRC,IERR,IBIN
       CHARACTER*(*) CHOPT
 
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION WJI,AJI,WASUM
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL WJI,AJI,WASUM
#endif
 
C Some variables for use with HGIVE
       CHARACTER*80 CHTITL
       INTEGER NY,NWT,LOC
       REAL XMI,XMA,YMI,YMA
 
       LOGICAL LWTHIS
       INTEGER IDWTH,NWTHIS,NCXD,NCXM,JSRC,JWT
       PARAMETER(IDWTH=12345)
       REAL DUMWT,CNORM,FACT
       PARAMETER(DUMWT=1.0)
 
       INTEGER IRET,INORM,IFIRST
 
       LOGICAL HEXIST
       EXTERNAL HEXIST
       DATA IFIRST/0/
 
       IF(IFIRST.EQ.0)THEN
          WRITE(6,*)' *********************************************'
          WRITE(6,*)' *** THERE IS A NEW VERSION OF HMCLNL      ***'
          WRITE(6,*)' *** YOU MUST NOW PASS THE HISTOGRAM IDS   ***'
          WRITE(6,*)' *** TO ALLOW FOR MULTIPLE SIMULTANEOUS    ***'
          WRITE(6,*)' *** FITS - HMCINI MUST ALSO BE CALLED     ***'
          WRITE(6,*)' *** FOR EACH SET OF HISTOGRAMS IF A       ***'
          WRITE(6,*)' *** SIMULTANEOUS FIT IS BEING PERFORMED   ***'
          WRITE(6,*)' *** THE NEW HMCLNL IS USED LIKE THIS:     ***'
          WRITE(6,*)' ***                                       ***'
          WRITE(6,*)' *** RLNL=HMCLNL(IDDATA,IDMC,IDWT,NSRC,PJ) ***'
          WRITE(6,*)' ***                                       ***'
          WRITE(6,*)' *** THE OLD VERSION WILL CONTINUE TO BE   ***'
          WRITE(6,*)' *** AVAILABLE AS HMCLNO(PJ) FOR A COUPLE  ***'
          WRITE(6,*)' *** OF MONTHS.                            ***'
          WRITE(6,*)' ***                                       ***'
          WRITE(6,*)' *** REPORT PROBLEMS TO BEESTON@CERNVM     ***'
          WRITE(6,*)' *********************************************'
          IFIRST=0
       ENDIF
C Initialise error flag
       IERR=0
C put number of sources in common
       NMCSRC=NSRC
 
       LWTHIS=.FALSE.
 
C Decode options
       CALL HUOPTC(CHOPT,'FLWSNPE',IOPT)
 
 
C Initial checks - first check number of MC histos doesnt exceed NSRCMX
       IF(NMCSRC.GT.NSRCMX)THEN
          CALL HBUG('Number of MC histograms exceeds maximum of 20',
     +'HMCINI',0)
          IERR=1
          GOTO 99
       ENDIF
       IF(NMCSRC.LT.NSRCMN)THEN
          CALL HBUG('Number of MC histograms less than minimum of 2',
     +'HMCINI',0)
          IERR=1
          GOTO 99
       ENDIF
 
C Next get links to histos, stop if any histo cant be found
       IF(HEXIST(IDDATA))THEN
          IDD=IDDATA
       ELSE
          CALL HBUG('Data histogram missing','HMCINI',IDDATA)
          IERR=1
          GOTO 99
       ENDIF
       DO 140 JSRC=1,NMCSRC
          IF(HEXIST(IDMC(JSRC)))THEN
             IDM(JSRC)=IDMC(JSRC)
          ELSE
             CALL HBUG('MC histogram missing','HMCINI',IDMC(JSRC))
             IERR=1
             GOTO 99
          ENDIF
140    CONTINUE
C check all histos have entries
       NDATEV=NINT(HSUM(IDD))
       IF(NDATEV.LE.0)THEN
          CALL HBUG('Data histogram has no entries - cant fit',
     +'HMCINI',IDD)
          IERR=1
          GOTO 99
       ENDIF
       DO 150 JSRC=1,NMCSRC
          NMCEV(JSRC)=NINT(HSUM(IDM(JSRC)))
          IF(NMCEV(JSRC).LE.0)THEN
             CALL HBUG('MC histogram has no entries - cant fit',
     +'HMCINI',IDM(JSRC))
             IERR=1
             GOTO 99
          ENDIF
150    CONTINUE
 
 
C check all histos have the same number of bins
 
       CALL HGIVE(IDD,CHTITL,NCXD,XMI,XMA,NY,YMI,YMA,NWT,LOC)
       DO 160 JSRC=1,NMCSRC
          CALL HGIVE(IDM(JSRC),CHTITL,NCXM,XMI,XMA,NY,YMI,YMA,NWT,LOC)
          IF(NCXM.NE.NCXD)THEN
             CALL HBUG(
     +'MC histo has different number of bins to data histo',
     +'HMCINI',IDM(JSRC))
             IERR=1
             GOTO 99
          ENDIF
160    CONTINUE
       NTOT=NCXD
 
C Weight histograms - check existence, if histograms not provided then
C use 12345+jwt (filled with 1).  If 'W' option but no histos provided, use
C 12345 for all but issue warning message
C ..first find an unused id for the dummy weight histogram
       JWT=0
165    IF(HEXIST(IDWTH+JWT))THEN
          JWT=JWT+1
          GOTO 165
       ENDIF
 
C now check existence of histograms if weight option chosen
       IF(IOPT(3).EQ.1)THEN
          NWTHIS=0
          DO 170 JSRC=1,NMCSRC
             IF(.NOT.HEXIST(IDWT(JSRC)))THEN
                CALL HBUG(
     +'Weight histo does not exist - using default',
     +'HMCINI',IDW(JSRC))
                IDW(JSRC)=IDWTH+JWT
                LWTHIS=.TRUE.
             ELSE
                IDW(JSRC)=IDWT(JSRC)
                NWTHIS=NWTHIS+1
             ENDIF
170       CONTINUE
       ELSE
          DO 180 JSRC=1,NMCSRC
             IDW(JSRC)=IDWTH+JWT
             LWTHIS=.TRUE.
180       CONTINUE
       ENDIF
C Put new wt histo ids in idwt to pass back to user
       DO 185 JSRC=1,NMCSRC
          IDWT(JSRC)=IDW(JSRC)
185    CONTINUE
 
C book + fill the default weights histogram.
       IF(LWTHIS)THEN
          CALL HBOOK1(IDWTH+JWT,'Dummy weight histo',NTOT,0.5,
     +    FLOAT(NTOT)+0.5,0.)
          DO 190 JBIN=1,NTOT
             CALL HFILL(IDWTH+JWT,FLOAT(JBIN),0.,DUMWT)
190       CONTINUE
       ENDIF
 
C Check all weights histos have right number of bins
       DO 200 JSRC=1,NMCSRC
          CALL HGIVE(IDW(JSRC),CHTITL,NCXM,XMI,XMA,NY,YMI,YMZ,NWT,LOC)
          IF(NCXM.NE.NTOT)THEN
             CALL HBUG(
     +'Weight histo has different number of bins to data histo',
     +'HMCINI',IDM(JSRC))
             IERR=1
             GOTO 99
          ENDIF
200    CONTINUE
 
C Now normalise weight histograms - first calculate overall normalisation
C factors
       DO 210 JSRC=1,NMCSRC
          IRET=3
          CALL HLOOP(IDW(JSRC),'HMCINI',IRET)
          IF(IRET.EQ.0)THEN
             CALL HBUG(
     +'Weight histogram does not exist ','HMCINI',IDW(JSRC))
             STOP
          ELSE
             INORM=JBIT(IQ(LCID+KBITS),18)
          ENDIF
 
* only normalise histograms once!
          IF(INORM.EQ.0)THEN
            WASUM=0.0D0
            DO 220 IBIN=1,NTOT
               WJI=HI(IDW(JSRC),IBIN)
               AJI=HI(IDM(JSRC),IBIN)
               WASUM=WASUM+WJI*AJI
220         CONTINUE
            BJ(JSRC)=WASUM/FLOAT(NMCEV(JSRC))
            IF(BJ(JSRC).LE.0.0)THEN
               IERR=1
               CALL HBUG(
     +'Normalisation constant for weight histo negative or zero',
     +'HMCINI',IDW(JSRC))
               GOTO 99
            ENDIF
            CNORM=1.0/(BJ(JSRC))
            CALL HOPERA(IDW(JSRC),'+',IDW(JSRC),IDW(JSRC),CNORM,0.0)
* musnt pass double precision to hnorma
            FACT=BJ(JSRC)
C Pack normalisation constant bj in histogram using HNORMA
            CALL HNORMA(IDW(JSRC),FACT)
 
          ENDIF
210    CONTINUE
 
99     CONTINUE
 
 
 
       RETURN
       END
 
