*
* $Id: frpacol.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frpacol.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C********************************* END FRPPCOL *************************
 
C********************************* FRPACOL *****************************
 
          SUBROUTINE FRPACOL
 
C --- this routine deals with p-A collisions working in the rest frame
C      of the target center and taking z axis parallel to the projectile
C      incident direction
 
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRGEOMC/NFLG,NUMROP,NUMROT,NUMREP
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      SAVE
      DIMENSION CORT(KSZ2,3)
 
50    IF (NFLG.NE.0) GOTO 100
C ==> first entry
      NFLG=1
      NFLG2=0
 
C --- initialization
C.....RMIN is the minimum distance required between two nucleons.
 
      RMIN=VFR(3)
      RMIN2=RMIN*RMIN
      CUTOFF=AOP(7)*3.
C --- parameters of the nucleon density distribution
      NA=IOP(5)
        IF (KFR(6).EQ.0.OR.(KFR(6).EQ.2.AND.IOP(5).LE.79) ) THEN
         CALL FRSEARC(2,FMMT,RMMT)
        ELSE
         CALL FRNUCDF(NA,A0,A2,A4,RMAX3)
        ENDIF
 
100   IF (NFLG2.NE.0) GOTO 120
C ==> second entry
      NFLG2=1
      NFLG3=0
      NROT=0
 
C --- determine the coordinates of target nucleons
      NA=IOP(5)
        IF (KFR(6).EQ.0.OR.(KFR(6).EQ.2.AND.IOP(5).LE.79) ) THEN
          CALL FRNUCOR(2,NA,RMIN2,FMMT,RMMT,CORT)
        ELSE
          CALL FRNUCOD(NA,RMIN2,A0,A2,A4,RMAX3,CORT)
        ENDIF
 
120   IF (NFLG3.NE.0) GOTO 150
C ==> third entry
      NFLG3=1
      NREP=0
      NROT=NROT+1
 
        IF (KFR(6).EQ.0.OR.(KFR(6).EQ.2.AND.IOP(5).LE.79) ) THEN
C --- rotate the target nucleus 90 degrees
      DO 130 I=1,IOP(5)
      W=CORT(I,1)
      CORT(I,1)=CORT(I,2)
      CORT(I,2)=CORT(I,3)
      CORT(I,3)=W
130   CONTINUE
          ELSE
C --- rotate target nucleus randomly along each axis with probabity 1/3
C     it is checked that this rotation gives an even solid angle dist.
      CDELTA=-1.+2.*RLU(0)
      SDELTA=FRSQR(MAX(0.,1.-CDELTA**2), 'SDELCA' )
      RA=RLU(0)
        IF (RA.GT.0.6666667) THEN
C --- rotate aronud z axis
      DO 132 I=1,IOP(5)
      X=CORT(I,1)*CDELTA-CORT(I,2)*SDELTA
      CORT(I,2)=CORT(I,1)*SDELTA+CORT(I,2)*CDELTA
      CORT(I,1)=X
132   CONTINUE
        ELSE IF (RA.GT.0.3333333) THEN
C --- rotate aronud x axis
      DO 134 I=1,IOP(5)
      Y=CORT(I,2)*CDELTA-CORT(I,3)*SDELTA
      CORT(I,3)=CORT(I,2)*SDELTA+CORT(I,3)*CDELTA
      CORT(I,2)=Y
134   CONTINUE
        ELSE
C --- rotate aronud y axis
      DO 136 I=1,IOP(5)
      Z=CORT(I,3)*CDELTA-CORT(I,1)*SDELTA
      CORT(I,1)=CORT(I,3)*SDELTA+CORT(I,1)*CDELTA
      CORT(I,3)=Z
136   CONTINUE
        ENDIF
          ENDIF
 
C --- find out the scope in X-Y plane of target nucleus
      XMAXT=CORT(1,1)
      XMINT=XMAXT
      YMAXT=CORT(1,2)
      YMINT=YMAXT
        DO 140 I=2,IOP(5)
      IF (CORT(I,1).GE.XMAXT) XMAXT=CORT(I,1)
      IF (CORT(I,1).LE.XMINT) XMINT=CORT(I,1)
      IF (CORT(I,2).GE.YMAXT) YMAXT=CORT(I,2)
      IF (CORT(I,2).LE.YMINT) YMINT=CORT(I,2)
140     CONTINUE
 
C --- target area in X-Y plane to be shooted
      XMAX=XMAXT+CUTOFF
      XMIN=XMINT-CUTOFF
      YMAX=YMAXT+CUTOFF
      YMIN=YMINT-CUTOFF
 
      IF (NROT.EQ.NUMROT) NFLG2=0
 
C ==> fourth entry
150   NREP=NREP+1
 
C --- sample impact, (XP,YP), of projectile
        IF (KFR(3).EQ.2.OR.KFR(3).EQ.3) THEN
      BPRO=FRSQR(RLU(0)*(VFR(2)*VFR(2)-VFR(1)*VFR(1))
     >                    +VFR(1)*VFR(1),'BPRO09')
      BPHI=6.2832*RLU(0)
      XP=BPRO*COS(BPHI)
      YP=BPRO*SIN(BPHI)
        ELSE
      XP=(XMAX-XMIN)*RLU(0)+XMIN
      YP=(YMAX-YMIN)*RLU(0)+YMIN
        ENDIF
      AOP(2)=FRSQR(XP**2+YP**2,'bipa22')
 
      IOP(2)=0
      IOP(9)=0
      IOP(10)=0
 
          DO 200 I=1,IOP(5)
C --- distance between the projectile proton and a target nucleon
      R2=(XP-CORT(I,1))**2+(YP-CORT(I,2))**2
C --- judge if a binary collision takes place
 
      PP = FRVOV(R2)
 
        IF (RLU(0).LT.PP) THEN
      IOP(2)=IOP(2)+1
      IOP(10)=IOP(10)+1
      IF (IOP(2).GT.3000) CALL FRMGOUT(0,0,'Array NUC() needs to be
     > expanded (3000 not enough)', 0.,0.,0.,0.,0.)
      NUC(1,IOP(2))=1
      NUC(2,IOP(2))=I
        ENDIF
200   CONTINUE
 
      IF (IOP(10).GT.0) IOP(9)=1
 
      IF (NREP.EQ.NUMREP) NFLG3=0
 
      IF ((KFR(3).EQ.1.OR.KFR(3).EQ.3).AND.IOP(9).LT.IOP(3)) GOTO 50
      IF (IOP(9).EQ.0) GOTO 50
 
C --- make order numbers of wounded target nucleons tightly
      DO 770 I=1,IOP(5)
        II=999
        DO 750 J=1,IOP(2)
        IF (NUC(2,J).GE.I.AND.NUC(2,J).LT.II) II=NUC(2,J)
750     CONTINUE
        IF (II.EQ.I) GOTO 770
        IF (II.EQ.999) GOTO 780
        DO 760 J=1,IOP(2)
        IF (NUC(2,J).EQ.II) THEN
          IDN(2,I)= IDN(2,II)
          FMN(2,I)= FMN(2,II)
          NUC(2,J)=I
        ENDIF
760     CONTINUE
770   CONTINUE
780   CONTINUE
 
      RETURN
      END
