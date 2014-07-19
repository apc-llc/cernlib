*
* $Id: qjetfr.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: qjetfr.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE QJETFR(IN,NTPNH)
C.----------------------------------------------------------------------
C.
C.    THIS SUBROUTINE IMPLEMENTS THE FRAGMENTATION OF A QUARK / DIQUARK
C.    WHICH ENERGY, MOMENTUM AND FLAVOUR ARE GIVEN. NOTE THAT AN INDEP.
C.    JET FRAGMENTATION MODEL IS USED TO FRAGMENT THE PARTON. THE
C.    RESULT CONTAINS MESONS, BARYONS AND A LEFT-OVER PARTON WHICH IS
C.    NEEDED FOR OVERALL QUANTUM NUMBER CONSERVATION.
C.    LAST UPDATE: 10/04/89
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION YLQU,YHVY
#endif
#include "eurodec/eudopt.inc"
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
#include "eurodec/hadgen.inc"
#include "eurodec/result.inc"
#include "eurodec/frgfix.inc"
#include "eurodec/quafix.inc"
#include "eurodec/picons.inc"
      DIMENSION PQQ(2),PQN(2)
C--
C--   KEEP INFORMATION ON PARTON FOR LATER JET ROTATION
      CT=PHA(3,IN)/PHA(5,IN)
      ST=1.-CT**2
      IF (ST.LE.0.) THEN
        ST=0.0
        CF=SIGN(1.,PHA(3,IN))
        SF=0.0
      ELSE
        ST=SQRT(ST)
        CF=PHA(1,IN)/PHA(5,IN)/ST
        SF=PHA(2,IN)/PHA(5,IN)/ST
      ENDIF
C--
C--   START LOOP OVER PRIMARY MESONS AND BARYONS
C--   SUM OF PARTICLE MASSES SHOULD NOT EXCEED ENERGY!
      NTFIRS=NTPNH+1
   10 SUMM=0.
      ID=IH(IN)
      EPLP=PHA(4,IN)+PHA(5,IN)
      EPLPR=EPLP
      PQQ(1)=0.
      PQQ(2)=0.
      DO 30 N=1,NTMAX
        NTPNH=NTPNH+1
        IF (NTPNH.GT.NTMAX) CALL ERRORD(61,' ',FLOAT(NTMAX))
        IDABS=IABS(ID)
        IF (IDABS.LT.9) THEN
C--
C--   CHOOSE A MESON WITH THE PROBABILITY: PROMES
          IF (EURRAN(N).GT.PROMES) THEN
            INDEXT(NTPNH)=IDIQRK(ID,IQ2,IQ3,0)
            IDLAST=ID
            ID=-(IQ2*10+IQ3)
          ELSE
            INDEXT(NTPNH)=IQQBAR(ID,IQ2)
            IDLAST=ID
            ID=-IQ2
          ENDIF
          IDLM=IABS(IDLAST)
        ELSE
C--
C--   IQ1 AND IQ2 ARE ALREADY FIXED HERE...
          IQ1=ID/10
          IQ2=ID-IQ1*10
          INDEXT(NTPNH)=IDIQRK(IQ1,IQ2,IQ3,1)
          IDLAST=ID
          IDLM=MAX(IABS(IQ1),IABS(IQ2))
          ID=-IQ3
        ENDIF
        IPPOIN=ICONV(IABS(INDEXT(NTPNH)))
C--
C--     MASS SMEARING ACCORDING TO TRUNCATED BREIT-WIGNER (OPTIONAL)
        IF (MSMEAR.EQ.1) THEN
          PTEIL(5,NTPNH)=BWMASS(IPPOIN)
        ELSE
          PTEIL(5,NTPNH)=PM(IPPOIN)
        ENDIF
C--
C--   LONGITUDINAL FRAGMENTATION OF QUARKS...
        IDL=IABS(IDLAST)
        IF (IDL.LT.9) THEN
          IF (IDL.LE.3) THEN
            CALL DFUNRN(YLQU,Z)
          ELSE
            CALL DFUNRN(YHVY(1,IDL-3),Z)
          ENDIF
        ELSE
C--
C--   ...OR DIQUARKS
          IF (IDLM.LE.3) THEN
            CALL DFUNRN(YLQU,Z)
          ELSE
            CALL DFUNRN(YHVY(1,IDLM-3),Z)
          ENDIF
        ENDIF
C--
C--   GIVE HADRON TRANSVERSE MOMENTUM KICK...
        PHI=TWOPI*EURRAN(IPDUM)
        PT2=-2.*(Z*(1.-Z)/(1.-Z/2.))*LOG(MAX(EURRAN(IPTDUM),1.E-4))
        PT=SIGQ*SQRT(PT2)*2.3
        PQN(1)=PT*SIN(PHI)
        PQN(2)=PT*COS(PHI)
C--
C--   PT AND TRANSVERSE ENERGY OF MESON/BARYON
        AMT2=PTEIL(5,NTPNH)**2
        DO 20 I=1,2
          PTEIL(I,NTPNH)=PQQ(I)-PQN(I)
   20   AMT2=AMT2+PTEIL(I,NTPNH)**2
C--
C--   THE MINIMUM ENERGY FRACTION REQUIRED:
        IF (N.EQ.1) THEN
          Z0=MIN(SQRT(AMT2)/EPLP,1.)
        ELSE
          Z0=0.
        ENDIF
        Z=Z0+(1.-Z0)*Z
        B=MAX(Z*EPLP,1.E-5)
        A=AMT2/B
C--
C--   LONGITUDINAL MOM. WITH RESPECT TO THE PARENT AXIS, ENERGY
C--   REMOVE BACKWARDS MOVING PARTICLES...
        PTEIL(3,NTPNH)=0.5*(B-A)
        IF ((PTEIL(3,NTPNH).GT.0.).OR.((N.EQ.1).AND.(IDLM.GE.4))) THEN
          PTEIL(4,NTPNH)=0.5*(B+A)
          IF (NTPNH.EQ.NTFIRS) THEN
            ITHEL(NTPNH)=IHH(IN)
            IF (IHH(IN).NE.0) THEN
              IF (EURRAN(IDEDUM).LE.QDEPOL) ITHEL(NTPNH)=0
            ENDIF
          ELSE
            ITHEL(NTPNH)=0
          ENDIF
          PQQ(1)=PQN(1)
          PQQ(2)=PQN(2)
          EPLPR=(1.-Z)*EPLP
          SUMM=SUMM+PTEIL(5,NTPNH)
        ELSE
          NTPNH=NTPNH-1
          ID=IDLAST
        ENDIF
C--
C--   REDUCE THE REMAINING (E+P//)
        EPLP=(1.-Z)*EPLP
C--
C--   ALLOW LARGER MULTIPLICITY FOR SMALL ENERGY OBJECTS
        IF (N.EQ.1) THEN
          IF (EPLP.LE.EQUMIN(1)) GOTO 40
        ELSE
          IF (EPLP.LE.EQUMIN(2)) GOTO 40
        ENDIF
   30 CONTINUE
C--
C--   STOP FRAGMENTING, REMAINING  ENERGY NOT SUFFICIENT...
C--   ...STORE PROPERTIES OF LAST QUARK/DIQUARK IN /HADGEN/
   40 IF ((SUMM.GE.PHA(4,IN)).AND.(NTPNH.NE.NTFIRS)) THEN
        NTPNH=NTFIRS-1
        GOTO 10
      ENDIF
      IDABS=ABS(ID)
      AHM(IN)=PM(ICONV(IDABS))
      IH(IN)=ID
      PT2=PQQ(1)**2+PQQ(2)**2
      AM2=AHM(IN)**2
      IF ((EPLPR-SQRT(PT2)).GT.(SQRT(PT2+AM2))) THEN
        PHA(1,IN)=PQQ(1)
        PHA(2,IN)=PQQ(2)
        PHA(4,IN)=(EPLPR**2+AM2)/EPLPR/2.
        PHA(3,IN)=MAX((PHA(4,IN)**2-AM2-PT2),0.)
        PHA(5,IN)=SQRT(PT2+PHA(3,IN))
        PHA(3,IN)=SQRT(PHA(3,IN))
      ELSE
        ALPHA2=MIN(EPLPR**2/(SQRT(PT2*(PT2+AM2))+2.*PT2+AM2),1.)
        ALPHA1=EURRAN(IALDUM)
        PHA(1,IN)=SQRT((1.-ALPHA1)*ALPHA2)*PQQ(1)
        PHA(2,IN)=SQRT((1.-ALPHA1)*ALPHA2)*PQQ(2)
        PT2=ALPHA2*PT2
        PHA(4,IN)=SQRT(PT2+AM2)
        PHA(3,IN)=SQRT(ALPHA1*PT2)
        PHA(5,IN)=SQRT(PT2)
      ENDIF
C--
C--   WE ROTATE THE JET IN THE ORIGINAL PARTON DIRECTION...
      DO 50 I=NTFIRS,NTPNH
   50 CALL ROTER(CT,ST,CF,SF,PTEIL(1,I))
      CALL ROTER(CT,ST,CF,SF,PHA(1,IN))
      RETURN
      END
