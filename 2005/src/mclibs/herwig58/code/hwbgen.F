*
* $Id: hwbgen.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbgen.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWBGEN.
*CMZ :-        -26/04/91  14.15.56  by  Federico Carminati
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWBGEN
C-----------------------------------------------------------------------
C     BRANCHING GENERATOR WITH INTERFERING GLUONS
C-----------------------------------------------------------------------
C     HWBGEN EVOLVES QCD JETS ACCORDING TO THE METHOD OF
C     G.MARCHESINI & B.R.WEBBER, NUCL. PHYS. B238(1984)1
C-----------------------------------------------------------------------
C     MODIFICATIONS FROM ORIGINAL (BY MHS):
C     TREATS SPECTATOR W/Z/H THE SAME AS SPECTATOR LEPTONS
C     ADDS EXTRA GLUONS/PHOTONS INTO THE `DEAD ZONE' IN E+E- SHOWERING
C     REVISED 9/12/92 TO INCLUDE NMXJET
C-----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      DOUBLE PRECISION HWULDO,HWRGAU,EINHEP,ERTXI,RTXI,XF
      INTEGER NTRY,LASHEP,IHEP,NRHEP,ID,IST,JHEP,KPAR,I,J,
     & IRHEP(NMXJET),IRST(NMXJET)
      LOGICAL HWRLOG
      IF (IERROR.NE.0) RETURN
      IF (IPRO.EQ.80) RETURN
C---CHECK THAT EMSCA IS SET
      IF (EMSCA.LE.ZERO) CALL HWWARN('HWBGEN',200,*999)
      IF (HARDME) THEN
C---FORCE A BRANCH INTO THE `DEAD ZONE' IN E+E-
        IF (IPROC/10.EQ.10) CALL HWBDED(1)
C---FORCE A BRANCH INTO THE `DEAD ZONE' IN DIS
        IF (IPRO.EQ.90) CALL HWBDIS(1)
      ENDIF
      NTRY=0
      LASHEP=NHEP
   10 NTRY=NTRY+1
      IF (NTRY.GT.NETRY) CALL HWWARN('HWBGEN',100,*999)
      NRHEP=0
      NHEP=LASHEP
      FROST=.FALSE.
      DO 100 IHEP=1,LASHEP
      IST=ISTHEP(IHEP)
      IF (IST.GE.111.AND.IST.LE.115) THEN
       NRHEP=NRHEP+1
       IRHEP(NRHEP)=IHEP
       IRST(NRHEP)=IST
       ID=IDHW(IHEP)
       IF (IST.NE.115) THEN
C---FOUND A PARTON TO EVOLVE
        NEVPAR=IHEP
        NPAR=2
        IDPAR(1)=17
        IDPAR(2)=ID
        TMPAR(1)=.TRUE.
        PPAR(2,1)=0.
        PPAR(4,1)=1.
        PPAR(5,1)=0.
        DO 15 J=1,2
        DO 15 I=1,2
        JMOPAR(I,J)=0
   15   JCOPAR(I,J)=0
C---SET UP EVOLUTION SCALE AND FRAME
        JHEP=JMOHEP(2,IHEP)
        IF (ID.EQ.13) THEN
          IF (HWRLOG(HALF)) JHEP=JDAHEP(2,IHEP)
        ELSEIF (IST.GT.112) THEN
          IF ((ID.GT.6.AND.ID.LT.13).OR.
     &        (ID.GT.214.AND.ID.LT.221)) JHEP=JDAHEP(2,IHEP)
        ELSE
          IF (ID.LT.7.OR.(ID.GT.208.AND.ID.LT.215)) JHEP=JDAHEP(2,IHEP)
        ENDIF
        IF (JHEP.EQ.0) THEN
          CALL HWWARN('HWBGEN',1,*999)
          JHEP=IHEP
        ENDIF
        JCOPAR(1,1)=JHEP
        EINHEP=PHEP(4,IHEP)
        ERTXI=HWULDO(PHEP(1,IHEP),PHEP(1,JHEP))
        IF (ERTXI.LT.0.) ERTXI=0.
        IF (IST.LE.112.AND.IHEP.EQ.JHEP) ERTXI=0.
        IF (ISTHEP(JHEP).EQ.152.OR.ISTHEP(JHEP).EQ.153) THEN
          ERTXI=ERTXI/PHEP(5,JHEP)
          RTXI=1.
        ELSE
          ERTXI=SQRT(ERTXI)
          RTXI=ERTXI/EINHEP
        ENDIF
        IF (RTXI.EQ.0.) THEN
          XF=1.
          PPAR(1,1)=0.
          PPAR(3,1)=1.
          PPAR(1,2)=EINHEP
          PPAR(2,2)=0.
          PPAR(4,2)=EINHEP
        ELSE
          XF=1./RTXI
          PPAR(1,1)=1.
          PPAR(3,1)=0.
          PPAR(1,2)=ERTXI
          PPAR(2,2)=1.
          PPAR(4,2)=ERTXI
        ENDIF
        IF (PPAR(4,2).LT.PHEP(5,IHEP)) PPAR(4,2)=PHEP(5,IHEP)
        IF (IST.GT.112) THEN
          TMPAR(2)=.TRUE.
          INHAD=0
          JNHAD=0
          XFACT=0.
        ELSE
          TMPAR(2)=.FALSE.
          JNHAD=IST-110
          INHAD=JNHAD
          IF (JDAHEP(1,JNHAD).NE.0) INHAD=JDAHEP(1,JNHAD)
          XFACT=XF/PHEP(4,INHAD)
          IF (PTRMS.NE.0.) THEN
C---GENERATE INTRINSIC PT
            PTINT(1,JNHAD)=HWRGAU(1,ZERO,PXRMS)
            PTINT(2,JNHAD)=HWRGAU(2,ZERO,PXRMS)
            PTINT(3,JNHAD)=PTINT(1,JNHAD)**2+PTINT(2,JNHAD)**2
          ELSE
            CALL HWVZRO(3,PTINT(1,JNHAD))
          ENDIF
        ENDIF
C---FOR QUARKS IN A COLOUR SINGLET, ALLOW SOFT MATRIX-ELEMENT CORRECTION
        HARDST=PPAR(4,2)
        IF (SOFTME.AND.IDHW(IHEP).LT.13.AND.
     $       JMOHEP(2,JHEP).EQ.IHEP.AND.JDAHEP(2,JHEP).EQ.IHEP) HARDST=0
C---CREATE BRANCHES AND COMPUTE ENERGIES
        DO 20 KPAR=2,NMXPAR
        IF (TMPAR(KPAR)) THEN
          CALL HWBRAN(KPAR)
        ELSE
          CALL HWSBRN(KPAR)
        ENDIF
        IF (IERROR.NE.0) RETURN
        IF (KPAR.EQ.NPAR) GO TO 30
   20   CONTINUE
C---COMPUTE MASSES AND 3-MOMENTA
   30   CONTINUE
        CALL HWBMAS
        IF (AZSPIN) CALL HWBSPN
        IF (TMPAR(2)) THEN
           CALL HWBTIM(2,1)
        ELSE
           CALL HWBSPA
        ENDIF
C---ENTER PARTON JET IN /HEPEVT/
        CALL HWBFIN(IHEP)
       ELSE
C---COPY SPECTATOR
        NHEP=NHEP+1
        IF (ID.GT.120.AND.ID.LT.133 .OR. ID.GE.198.AND.ID.LE.201) THEN
          ISTHEP(NHEP)=190
        ELSE
          ISTHEP(NHEP)=152
        ENDIF
        IDHW(NHEP)=ID
        IDHEP(NHEP)=IDPDG(ID)
        JMOHEP(1,NHEP)=IHEP
        JMOHEP(2,NHEP)=0
        JDAHEP(2,NHEP)=0
        JDAHEP(1,IHEP)=NHEP
        CALL HWVEQU(5,PHEP(1,IHEP),PHEP(1,NHEP))
       ENDIF
       ISTHEP(IHEP)=ISTHEP(IHEP)+10
      ENDIF
  100 CONTINUE
C---COMBINE JETS
      ISTAT=20
      CALL HWBJCO
      IF (.NOT.FROST) THEN
C---ATTACH SPECTATORS
        ISTAT=30
        CALL HWSSPC
      ENDIF
      IF (FROST) THEN
C---BAD JET: RESTORE PARTONS AND RE-EVOLVE
         DO 120 I=1,NRHEP
  120    ISTHEP(IRHEP(I))=IRST(I)
         GO TO 10
      ENDIF
C---CONNECT COLOURS
      CALL HWBCON
      ISTAT=40
      IF (HARDME) THEN
C---CLEAN UP IF THERE WAS A BRANCH IN THE `DEAD ZONE' IN E+E-
        IF (IPROC/10.EQ.10) CALL HWBDED(2)
C---CLEAN UP IF THERE WAS A BRANCH IN THE `DEAD ZONE' IN DIS
        IF (IPRO.EQ.90) CALL HWBDIS(2)
      ENDIF
  999 END
