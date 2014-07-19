*
* $Id: hmclno.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hmclno.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/02 26/04/94  21.56.24  by  Unknown
*-- Author :    Roger Barlow, Christine Beeston   24/09/93
 
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION FUNCTION HMCLNO(PJ)
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL FUNCTION HMCLNO(PJ)
#endif
 
* HMCLNO returns the log likelihood (including effect of both data
* and Monte Carlo statistics) that the data distribution arose from a
* distribution given by combining the Monte Carlo distributions, weighted
* by the weights provided, using the fractions given in FRAC.
* HMCINI must be called before this function may be used.
*
* Input parameters:
* ----------------
* FRAC: Double precision array of dimension NMCSRC containing the
* fraction of each Monte Carlo distribution you wish to assume is in the data
* distribution, in order to calculate the log likelihood.
 
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcmcpm.inc"
 
 
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION ANSWER(NSRCMX), X(NSRCMX), AKI, TI,
     +                  F,AJI,WJI,PJ(NSRCMX),PJSUM,BPJSUM
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL ANSWER(NSRCMX), X(NSRCMX), AKI, TI,
     +                  F,AJI,WJI,PJ(NSRCMX),PJSUM,BPJSUM
#endif
 
       INTEGER KZERO,I,J,IRET,IFIRST
       DATA IFIRST/0/
 
       IF(IFIRST.EQ.0)THEN
       WRITE(6,*)' ************************************************'
       WRITE(6,*)' ***   THIS IS THE OLD VERSION OF             ***'
       WRITE(6,*)' ***   HMCLNL:  IT WILL BE DELETED IN         ***'
       WRITE(6,*)' ***   A COUPLE OF MONTHS.  THE NEW           ***'
       WRITE(6,*)' ***   VERSION IS NO SLOWER AND               ***'
       WRITE(6,*)' ***   ALLOWS FOR MULTIPLE SIMULTANEOUS       ***'
       WRITE(6,*)' ***   FITS.  THE HISTOGRAM IDS MUST BE       ***'
       WRITE(6,*)' ***   PASSED AS FOLLOWS:                     ***'
       WRITE(6,*)' ***                                          ***'
       WRITE(6,*)' ***   RLNL=HMCLNL(IDDATA,IDMC,IDWT,NSRC,PJ)  ***'
       WRITE(6,*)' ***                                          ***'
       WRITE(6,*)' *** PLEASE REPORT PROBLEMS TO BEESTON@CERNVM ***'
       WRITE(6,*)' ************************************************'
       IFIRST=1
       ENDIF
 
C retrieve histogram normalisation factors(bj) from weight histograms
       DO 20 J=1,NMCSRC
          BJ(J)=1.
          IRET=3
          CALL HLOOP(IDW(J),'HMCLNO',IRET)
          IF(IRET.EQ.0)THEN
             STOP
          ENDIF
          IF(I18.NE.0)THEN
             BJ(J)=Q(LCID+KNORM)
          ENDIF
20     CONTINUE
C get total numbers of events in histograms - ignoring under + overflows
       NDATEV=NINT(HSUM(IDD))
       DO 25 J=1,NMCSRC
          NMCEV(J)=NINT(HSUM(IDM(J)))
25     CONTINUE
 
C first convert Pj to Pj'
       PJSUM=0.0D0
       BPJSUM=0.0D0
       DO 30 J=1,NMCSRC
          PJSUM=PJSUM+PJ(J)
          BPJSUM=BPJSUM+BJ(J)*PJ(J)
30     CONTINUE
 
       DO 40 J=1,NMCSRC
          ANSWER(J)=BJ(J)*PJ(J)*PJSUM/BPJSUM
40     CONTINUE
 
C convert ANSWER to normalised fractions in X
       DO 50 J=1,NMCSRC
          X(J)=ANSWER(J)*NDATEV/NMCEV(J)
50     CONTINUE
 
       HMCLNO=0.0D0
 
       DO 80 I=1,NTOT
 
          CALL HADJUST(TI,I,X,KZERO,AKI)
          F=0.0
          DO 70 J=1,NMCSRC
             WJI=HI(IDW(J),I)
C now get AJI from the aji and the Ti
             IF(KZERO.NE.0.AND.X(J).EQ.X(KZERO))THEN
                AJI=AKI
             ELSE
                AJI=0
                IF(NINT(HI(IDM(J),I)).NE.0)THEN
                   AJI=HI(IDM(J),I)/(1.0D0+X(J)*WJI*TI)
                ENDIF
             ENDIF
 
             F=F+X(J)*WJI*AJI
 
             IF(NINT(HI(IDM(J),I)).NE.0)THEN
                IF(AJI.LE.0.0)THEN
                   CONTINUE
                ELSE
                   HMCLNO=HMCLNO+HI(IDM(J),I)*DLOG(AJI)
                ENDIF
             ENDIF
 
             HMCLNO=HMCLNO-AJI
70        CONTINUE
 
          IF(NINT(HI(IDD,I)).NE.0)THEN
             HMCLNO=HMCLNO+HI(IDD,I)*DLOG(F)
          ENDIF
 
          HMCLNO=HMCLNO-F
 
80     CONTINUE
 
       RETURN
       END
