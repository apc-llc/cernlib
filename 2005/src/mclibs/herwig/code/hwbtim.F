CDECK  ID>, HWBTIM.
*CMZ :-        -26/04/91  14.27.17  by  Federico Carminati
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWBTIM(INITBR,INTERF)
C-----------------------------------------------------------------------
C     Constructs full 4-momentum & production vertices in time-like jet
C     initiated by INITBR, interference partner INTERF and spin density
C     RHOPAR(INITBR). DECPAR(INITBR) returns jet's spin density matrix.
C     Includes azimuthal angular correlations between branching planes
C     due to spin (if AZSPIN) using the algorithm of Knowles & Collins.
C     Ses Nucl. Phys. B304 (1988) 794 & Comp. Phys. Comm. 58 (1990) 271.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,DMIN,PT,EIKON,EINUM,EIDEN1,EIDEN2,EISCR,
     & WT,SPIN,Z1,Z2,PRMAX,CAZ,CX,SX,ROHEP(3),RMAT(3,3),ZERO2(2)
      INTEGER INITBR,INTERF,IPAR,JPAR,KPAR,LPAR,MPAR,NTRY,JOLD
      LOGICAL EICOR,SWAP
      EXTERNAL HWRGEN
      DATA ZERO2,DMIN/ZERO,ZERO,1.D-15/
      IF (IERROR.NE.0) RETURN
      JPAR=INITBR
      KPAR=INTERF
      IF ((JDAPAR(1,JPAR).NE.0).OR.(IDPAR(JPAR).EQ.13)) GOTO 30
C No branching, assign decay matrix
      CALL HWVZRO(2,DECPAR(1,JPAR))
      RETURN
C Advance up the leader
C     Find the parent and partner of J
  10  IPAR=JMOPAR(1,JPAR)
      KPAR=JPAR+1
C Generate new Rho
      IF (JMOPAR(1,KPAR).EQ.IPAR) THEN
C        Generate Rho'
         CALL HWBAZF(IPAR,JPAR,PHIPAR(1,IPAR),RHOPAR(1,IPAR),
     &                                   ZERO2,RHOPAR(1,JPAR))
      ELSE
         KPAR=JPAR-1
         IF (JMOPAR(1,KPAR).NE.IPAR)
     &   CALL HWWARN('HWBTIM',100,*999)
C        Generate Rho''
         CALL HWBAZF(IPAR,KPAR,RHOPAR(1,IPAR),PHIPAR(1,IPAR),
     &                         DECPAR(1,KPAR),RHOPAR(1,JPAR))
      ENDIF
C Generate azimuthal angle of J's branching
  30  IF (JDAPAR(1,JPAR).EQ.0) THEN
C        Final state gluon
         CALL HWVZRO(2,DECPAR(1,JPAR))
         IF (JPAR.EQ.INITBR) RETURN
         GOTO 70
      ELSE
C Assign an angle to a branching using an M-function
C        Find the daughters of J
         LPAR=JDAPAR(1,JPAR)
         MPAR=JDAPAR(2,JPAR)
C Soft correlations
         CALL HWUROT(PPAR(1,JPAR), ONE,ZERO,RMAT)
         CALL HWUROF(RMAT,PPAR(1,KPAR),ROHEP)
         PT=MAX(SQRT(ROHEP(1)*ROHEP(1)+ROHEP(2)*ROHEP(2)),DMIN)
         EIKON=1.
         SWAP=.FALSE.
         EICOR=AZSOFT.AND.((IDPAR(LPAR).EQ.13).OR.(IDPAR(MPAR).EQ.13))
         IF (EICOR) THEN
C           Rearrange s.t. LPAR is the (softest) gluon
            IF (IDPAR(MPAR).EQ.13) THEN
               IF (IDPAR(LPAR).NE.13.OR.
     &             PPAR(4,MPAR).LT.PPAR(4,LPAR)) THEN
                  SWAP=.TRUE.
                  LPAR=MPAR
                  MPAR=LPAR-1
               ENDIF
            ENDIF
            EINUM=(PPAR(4,KPAR)*PPAR(4,LPAR))
     &        *ABS(PPAR(2,LPAR)-PPAR(2,MPAR))
            EIDEN1=(PPAR(4,KPAR)*PPAR(4,LPAR))-ROHEP(3)*PPAR(3,LPAR)
            EIDEN2=PT*ABS(PPAR(1,LPAR))
            EISCR=1.-(PPAR(5,MPAR)/PPAR(4,MPAR))**2
     &           /MIN(PPAR(2,LPAR),PPAR(2,MPAR))
            EIKON=EISCR+EINUM/MAX(EIDEN1-EIDEN2,DMIN)
         ENDIF
C Spin correlations
         WT=0.
         SPIN=1.
         IF (AZSPIN) THEN
            Z1=PPAR(4,LPAR)/PPAR(4,JPAR)
            Z2=1.-Z1
            IF (IDPAR(JPAR).EQ.13.AND.IDPAR(LPAR).EQ.13) THEN
               WT=Z1*Z2/(Z1/Z2+Z2/Z1+Z1*Z2)
            ELSEIF (IDPAR(JPAR).EQ.13.AND.IDPAR(LPAR).LT.13) THEN
               WT=-2.*Z1*Z2/(Z1*Z1+Z2*Z2)
            ENDIF
         ENDIF
C Assign the azimuthal angle
         PRMAX=(1.+ABS(WT))*EIKON
         NTRY=0
   50    NTRY=NTRY+1
         IF (NTRY.GT.NBTRY) CALL HWWARN('HWBTIM',101,*999)
         CALL HWRAZM( ONE,CX,SX)
         CALL HWUROT(PPAR(1,JPAR),CX,SX,RMAT)
C Determine the angle between the branching planes
         CALL HWUROF(RMAT,PPAR(1,KPAR),ROHEP)
         CAZ=ROHEP(1)/PT
         PHIPAR(1,JPAR)=2.*CAZ*CAZ-1.
         PHIPAR(2,JPAR)=2.*CAZ*ROHEP(2)/PT
         IF (EICOR) EIKON=EISCR+EINUM/MAX(EIDEN1-EIDEN2*CAZ,DMIN)
         IF (AZSPIN) SPIN=1.+WT*(RHOPAR(1,JPAR)*PHIPAR(1,JPAR)
     &                          +RHOPAR(2,JPAR)*PHIPAR(2,JPAR))
         IF (SPIN*EIKON.LT.HWRGEN(0)*PRMAX) GOTO 50
C Construct full 4-momentum of L and M
         JOLD=JPAR
         IF (SWAP) THEN
           PPAR(1,LPAR)=-PPAR(1,LPAR)
           PPAR(1,MPAR)=-PPAR(1,MPAR)
           JPAR=MPAR
         ELSE
           JPAR=LPAR
         ENDIF
         PPAR(2,LPAR)=0.
         CALL HWUROB(RMAT,PPAR(1,LPAR),PPAR(1,LPAR))
         PPAR(2,MPAR)=0.
         CALL HWUROB(RMAT,PPAR(1,MPAR),PPAR(1,MPAR))
C Assign production vertex to L and M
         CALL HWUDKL(IDPAR(JOLD),PPAR(1,JOLD),VPAR(1,LPAR))
         CALL HWVSUM(4,VPAR(1,JOLD),VPAR(1,LPAR),VPAR(1,LPAR))
         CALL HWVEQU(4,VPAR(1,LPAR),VPAR(1,MPAR))
      ENDIF
  60  IF (JDAPAR(1,JPAR).NE.0) GOTO 10
C Assign decay matrix
      CALL HWVZRO(2,DECPAR(1,JPAR))
C Backtrack down the leader
  70  IPAR=JMOPAR(1,JPAR)
      KPAR=JDAPAR(1,IPAR)
      IF (KPAR.EQ.JPAR) THEN
C        Develop the side branch
         JPAR=JDAPAR(2,IPAR)
         GOTO 60
      ELSE
C        Construct decay matrix
         CALL HWBAZF(IPAR,KPAR,DECPAR(1,JPAR),DECPAR(1,KPAR),
     &                         PHIPAR(1,IPAR),DECPAR(1,IPAR))
      ENDIF
      IF (IPAR.EQ.INITBR) RETURN
      JPAR=IPAR
      GOTO 70
  999 END
