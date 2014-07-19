*
* $Id: hwualf.F,v 1.1.1.1 1996/03/08 17:02:17 mclareni Exp $
*
* $Log: hwualf.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:17  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.48  by  Unknown
*-- Author :
CDECK  ID>, HWUALF.
*CMZ :-        -15/07/92  14.08.45  by  Mike Seymour
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWUALF(IOPT,SCALE)
C     STRONG COUPLING CONSTANT
C     IOPT.EQ.0  INITIALIZES
C         .EQ.1  TWO-LOOP, FLAVOUR THRESHOLDS
C         .EQ.2  RATIO OF ABOVE TO ONE-LOOP
C                WITH 5-FLAVOUR BETA, LAMBDA=QCDL3
C         .EQ.3  ONE-LOOP WITH 5-FLAVOUR BETA, LAMBDA=QCDL3
C
C     MODIFIED 26/1/91 TO INCLUDE TOP THRESHOLD MATCHING
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION HWUALF
      DOUBLE PRECISION SCALE,KAFAC,B3,B4,B5,B6,C3,C4,C5,C6,C35,C45,C65,
     & D35,RHO,RAT,RLF,DRH,EPS
      INTEGER IOPT,ITN
      SAVE B3,B4,B5,B6,C3,C4,C5,C6,C35,C45,C65,D35
      DATA EPS/1.D-6/
      IF (IOPT.EQ.0) THEN
C---INITIALIZE CONSTANTS
        CAFAC=FLOAT(NCOLO)
        CFFAC=FLOAT(NCOLO**2-1)/(2.*CAFAC)
        B3=((11.*CAFAC)- 6.)/(12.*PIFAC)
        B4=((11.*CAFAC)- 8.)/(12.*PIFAC)
        B5=((11.*CAFAC)-10.)/(12.*PIFAC)
        B6=((11.*CAFAC)-12.)/(12.*PIFAC)
        BETAF=6.*PIFAC*B5
        C3=((17.*CAFAC**2)-(5.*CAFAC+3.*CFFAC)*3.)/(24.*PIFAC**2)/B3**2
        C4=((17.*CAFAC**2)-(5.*CAFAC+3.*CFFAC)*4.)/(24.*PIFAC**2)/B4**2
        C5=((17.*CAFAC**2)-(5.*CAFAC+3.*CFFAC)*5.)/(24.*PIFAC**2)/B5**2
        C6=((17.*CAFAC**2)-(5.*CAFAC+3.*CFFAC)*6.)/(24.*PIFAC**2)/B6**2
        KAFAC=CAFAC*(67./18.-PIFAC**2/6.)-25./9.
C---QCDLAM IS 5-FLAVOUR LAMBDA-MS-BAR AT LARGE X OR Z
C---QCDL5  IS 5-FLAVOUR LAMBDA-MC
        QCDL5=QCDLAM*EXP(KAFAC/(4.*PIFAC*B5))/SQRT(2.D0)
C---COMPUTE THRESHOLD MATCHING
        RHO=2.*LOG(RMASS(6)/QCDL5)
        RAT=LOG(RHO)/RHO
        C65=(B5/(1.-C5*RAT)-B6/(1.-C6*RAT))*RHO
        RHO=2.*LOG(RMASS(5)/QCDL5)
        RAT=LOG(RHO)/RHO
        C45=(B5/(1.-C5*RAT)-B4/(1.-C4*RAT))*RHO
        RHO=2.*LOG(RMASS(4)/QCDL5)
        RAT=LOG(RHO)/RHO
        C35=(B4/(1.-C4*RAT)-B3/(1.-C3*RAT))*RHO+C45
C---FIND QCDL3
        D35=-1./(B3*C35)
        DO 10 ITN=1,100
          RAT=LOG(D35)/D35
          RLF=B3*D35/(1.-C3*RAT)
          DRH=B3*(RLF+C35)*D35**2/((1.-2.*C3*RAT+C3/D35)*RLF**2)
          D35=D35-DRH
          IF (ABS(DRH).LT.EPS*D35) GO TO 20
   10   CONTINUE
   20   QCDL3=QCDL5*EXP(0.5*D35)
      ENDIF
      IF (SCALE.LE.QCDL5) CALL HWWARN('HWUALF',51,*999)
      RHO=2.*LOG(SCALE/QCDL5)
      IF (IOPT.EQ.3) THEN
        IF (RHO.LE.D35) CALL HWWARN('HWUALF',52,*999)
        HWUALF=1./(B5*(RHO-D35))
        RETURN
      ENDIF
      RAT=LOG(RHO)/RHO
      IF (SCALE.GT.RMASS(6)) THEN
        RLF=B6*RHO/(1.-C6*RAT)+C65
      ELSEIF (SCALE.GT.RMASS(5)) THEN
        RLF=B5*RHO/(1.-C5*RAT)
      ELSEIF (SCALE.GT.RMASS(4)) THEN
        RLF=B4*RHO/(1.-C4*RAT)+C45
      ELSE
        RLF=B3*RHO/(1.-C3*RAT)+C35
      ENDIF
      IF (RLF.LE.ZERO) CALL HWWARN('HWUALF',53,*999)
      IF (IOPT.EQ.1) THEN
        HWUALF=1./RLF
      ELSE
        HWUALF=B5*(RHO-D35)/RLF
        IF (HWUALF.GT.1.) CALL HWWARN('HWUALF',54,*999)
      ENDIF
      RETURN
 999  HWUALF=ZERO
      END
