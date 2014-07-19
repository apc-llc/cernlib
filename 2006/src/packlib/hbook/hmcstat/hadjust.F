*
* $Id: hadjust.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hadjust.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.20/13 19/10/93  15.35.31  by  Roger Barlow, Christine Beeston
*-- Author :    Roger Barlow, Christine Beeston   24/09/93
 
      SUBROUTINE HADJUST(TI,I,P,KZERO,AKI)
 
C Subroutine which given a set of Pj (answer), solves for the ti
C (ti = 1 - di/fi, where di is the number of data events in bin i,
C and fi is the number of mc events predicted in bin i, given the Pj
C provided.  fi = Nd * sum over j of (Pj aji/Nj), where Nd = total data
C events, Nj is total number of mc events from source j, aji is number
C of mc events from source j in bin i.
 
#include "hbook/hcmcpm.inc"
 
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION TI,P(NSRCMX),WPMAX,TMIN,AKI,
     + STEP,FUNC,DERIV,D,DELTA,WJI,WKI
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL TI,P(NSRCMX),WPMAX,TMIN,AKI,
     + STEP,FUNC,DERIV,D,DELTA,WJI,WKI
#endif
 
       INTEGER I,KZERO,IMAX,J,NPMAX,APMAXS
 
 
C set up largest step value
        STEP=0.2D0
 
C First case - di=0 -> ti=1
       IF(NINT(HI(IDD,I)).EQ.0)THEN
          TI=1.0D0
          KZERO=0
          RETURN
       ENDIF
 
C Find the largest pj (for case when one or more of the aji are zero)
       WPMAX=HI(IDW(1),I)*P(1)
       IMAX=1
       DO 10 J=2,NMCSRC
          WJI=HI(IDW(J),I)
          IF(WJI*P(J).GT.WPMAX)THEN
             WPMAX=WJI*P(J)
             IMAX=J
          ENDIF
10     CONTINUE
C count the number of sources having p=pmax, and the sum of their aji
       NPMAX=0
       APMAXS=0
       DO 20J=1,NMCSRC
          WJI=HI(IDW(J),I)
          IF(WJI*P(J).EQ.WPMAX)THEN
             NPMAX=NPMAX+1
             APMAXS=APMAXS+HI(IDM(J),I)
          ENDIF
C check that none of the p(j) are zero as this causes crashes
          IF(WJI*P(J).EQ.0.0D0)THEN
             WRITE(6,*)'ADJUST: P(',J,') =',P(J),' - zero
     + fractions cause crashes'
             WRITE(6,*)'ADJUST: dont set starting values
     + to zero - if you didnt then'
             WRITE(6,*)'ADJUST: try setting limits on the P(J)
     + (0<P(J)<1) before any SCAN or MIGRad calls'
          ENDIF
20     CONTINUE
 
       TMIN=-1./WPMAX
 
 
       KZERO=IMAX
       IF(APMAXS.EQ.0)THEN
          AKI=HI(IDD,I)/(1+WPMAX)
          WKI=HI(IDW(KZERO),I)
          DO 30 J=1,NMCSRC
             IF(J.NE.KZERO)THEN
                WJI=HI(IDW(J),I)
                IF(WKI*P(KZERO).NE.WJI*P(J))THEN
                   AKI=AKI-HI(IDM(J),I)*WJI*P(J)/
     +                     (WKI*P(KZERO)-WJI*P(J))
                ENDIF
             ENDIF
30        CONTINUE
          IF(AKI.GT.0)THEN
             AKI=AKI/NPMAX
             TI=TMIN
             RETURN
          ENDIF
       ENDIF
       KZERO=0
 
C Now zero cases have been sorted out, solve for the ti using
C Newton's method
       TI=0.
 
11     CONTINUE
 
C check starting value for ti is sensible, if not then zero is a good
C place to start.
       IF(TI.GE.1.0.OR.TI.LE.TMIN)THEN
          STEP=STEP/10.0
          TI=0.
       ENDIF
C value of function
       FUNC=-HI(IDD,I)/(1.0D0-TI)
       DERIV=FUNC/(1.0D0-TI)
 
       DO 40 J=1,NMCSRC
          WJI=HI(IDW(J),I)
          D=1.0D0/(TI+1.0D0/(WJI*P(J)))
          FUNC=FUNC+HI(IDM(J),I)*D
          DERIV=DERIV-HI(IDM(J),I)*D**2
40     CONTINUE
 
C check convergence - require either F small or step small
       IF(ABS(FUNC).GT.1.0D-12)THEN
          DELTA=-FUNC/DERIV
C limit the step size
          IF(DABS(DELTA).GT.STEP)THEN
             DELTA=DSIGN(STEP,DELTA)
          ENDIF
          TI=TI+DELTA
          IF(DABS(DELTA).GT.1.0D-13)THEN
             GOTO 11
          ENDIF
       ENDIF
 
       RETURN
       END
 
