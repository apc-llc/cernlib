*
* $Id: fraacol.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: fraacol.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C********************************* END FRPACOL ************************
 
C********************************* FRAACOL ******************************
 
       SUBROUTINE FRAACOL
 
C --- this subroutine deals with A-A collisions working in the rest
C       frame of the target center and taking Z axis parallel to the
C       projectile incident direction
 
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRGEOMC/NFLG,NUMROP,NUMROT,NUMREP
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      SAVE
      DIMENSION CORP(KSZ2,3),CORT(KSZ2,3),MARKP(KSZ2),MARKT(KSZ2)
 
50    IF (NFLG.NE.0) GOTO 100
C ==> first entry
      NFLG=1
      NFLG2=0
 
C --- initialization
      RMIN=VFR(3)
      RMIN2=RMIN*RMIN
      CUTOFF=AOP(7)*3.
C --- parameters of nucleon density distribution
      NA=IOP(3)
      CALL FRSEARC(1,FMMP,RMMP)
      NA=IOP(5)
        IF (KFR(6).EQ.0.OR.(KFR(6).EQ.2.AND.IOP(5).LE.79) ) THEN
            CALL FRSEARC(2,FMMT,RMMT)
        ELSE
        CALL FRNUCDF(NA,A0,A2,A4,RMAX3)
        ENDIF
 
100   IF (NFLG2.NE.0) GOTO 200
C ==> second entry
      NFLG2=1
      NFLG3=0
      NROTT=0
 
C --- determine the coordinates of target nucleons
      NA=IOP(5)
        IF (KFR(6).EQ.0.OR.(KFR(6).EQ.2.AND.IOP(5).LE.79) ) THEN
      CALL FRNUCOR(2,NA,RMIN2,FMMT,RMMT,CORT)
        ELSE
      CALL FRNUCOD(NA,RMIN2,A0,A2,A4,RMAX3,CORT)
        ENDIF
 
200   IF (NFLG3.NE.0) GOTO 300
C ==> third entry
      NFLG3=1
      NFLG4=0
      NROTT=NROTT+1
 
        IF (KFR(6).EQ.0.OR.(KFR(6).EQ.2.AND.IOP(5).LE.79) ) THEN
C --- rotate the target nucleus
        DO 210 I=1,IOP(5)
      WW=CORT(I,1)
      CORT(I,1)=CORT(I,2)
      CORT(I,2)=CORT(I,3)
      CORT(I,3)=WW
210     CONTINUE
          ELSE
C --- rotate target nucleus randomly along an axis with probabity 1/3
C     it is checked that this rotation gives an even solid angle dist.
      CDELTA=-1.+2.*RLU(0)
      SDELTA=FRSQR(MAX(0.,1.-CDELTA**2), 'SDE9A6')
      RA=RLU(0)
        IF (RA.GT.0.6666667) THEN
C --- rotate aronud z axis
      DO 212 I=1,IOP(5)
      X=CORT(I,1)*CDELTA-CORT(I,2)*SDELTA
      CORT(I,2)=CORT(I,1)*SDELTA+CORT(I,2)*CDELTA
      CORT(I,1)=X
212   CONTINUE
        ELSE IF (RA.GT.0.3333333) THEN
C --- rotate aronud x axis
      DO 214 I=1,IOP(5)
      Y=CORT(I,2)*CDELTA-CORT(I,3)*SDELTA
      CORT(I,3)=CORT(I,2)*SDELTA+CORT(I,3)*CDELTA
      CORT(I,2)=Y
214   CONTINUE
        ELSE
C --- rotate aronud y axis
      DO 216 I=1,IOP(5)
      Z=CORT(I,3)*CDELTA-CORT(I,1)*SDELTA
      CORT(I,1)=CORT(I,3)*SDELTA+CORT(I,1)*CDELTA
      CORT(I,3)=Z
216   CONTINUE
        ENDIF
          ENDIF
 
C --- find out the scope in X-Y plane of sampled target nucleons
      XMAXT=CORT(1,1)
      XMINT=XMAXT
      YMAXT=CORT(1,2)
      YMINT=YMAXT
        DO 220 I=2,IOP(5)
      IF (CORT(I,1).GE.XMAXT) XMAXT=CORT(I,1)
      IF (CORT(I,1).LE.XMINT) XMINT=CORT(I,1)
      IF (CORT(I,2).GE.YMAXT) YMAXT=CORT(I,2)
      IF (CORT(I,2).LE.YMINT) YMINT=CORT(I,2)
220     CONTINUE
 
      NROTP=0
C --- determine the coordinates of projectile nucleons
C       with respect to the rest frame of the projectile center
C       (Z axes of two frames are assumed to be parallel each other)
      NA=IOP(3)
      CALL FRNUCOR(1,NA,RMIN2,FMMP,RMMP,CORP)
 
      IF (NROTT.EQ.NUMROT) NFLG2=0
 
300   IF (NFLG4.NE.0) GOTO 400
C ==> fourth entry
      NFLG4=1
      NREP=0
      NROTP=NROTP+1
 
C --- rotate the projectile nucleus 90 degrees
        DO 310 I=1,IOP(3)
      WW=CORP(I,1)
      CORP(I,1)=CORP(I,2)
      CORP(I,2)=CORP(I,3)
      CORP(I,3)=WW
310     CONTINUE
 
C --- find out the scope of sampled projectile nucleons in thw X-Y plane
C       of the projectile rest frame
      XMAXP=CORP(1,1)
      XMINP=XMAXP
      YMAXP=CORP(1,2)
      YMINP=XMAXT
        DO 320 I=2,IOP(3)
      IF (CORP(I,1).GE.XMAXP) XMAXP=CORP(I,1)
      IF (CORP(I,1).LE.XMINP) XMINP=CORP(I,1)
      IF (CORP(I,2).GE.YMAXP) YMAXP=CORP(I,2)
      IF (CORP(I,2).LE.YMINP) YMINP=CORP(I,2)
320     CONTINUE
 
C --- start to treat the nucleus-nucleus collision
C --- first determine the area of the projectile nucleus shooting
C      with respect to the rest frame of the target center
      XMAX=XMAXT-XMINP+CUTOFF
      XMIN=XMINT-XMAXP-CUTOFF
      YMAX=YMAXT-YMINP+CUTOFF
      YMIN=YMINT-YMAXP-CUTOFF
 
      IF (NROTP.EQ.NUMROP) NFLG3=0
 
C ==> fifth entry
400   NREP=NREP+1
 
C --- sample impact of projectile with respect to the target rest frame
        IF (KFR(3).EQ.2.OR.KFR(3).EQ.3) THEN
      BPRO=FRSQR(RLU(0)*(VFR(2)*VFR(2)-VFR(1)*VFR(1))+
     >                      VFR(1)*VFR(1),'BPRO12')
      BPHI=6.2832*RLU(0)
      XPRO=BPRO*COS(BPHI)
      YPRO=BPRO*SIN(BPHI)
        ELSE
      XPRO=(XMAX-XMIN)*RLU(0)+XMIN
      YPRO=(YMAX-YMIN)*RLU(0)+YMIN
        ENDIF
      AOP(2)=FRSQR(XPRO**2+YPRO**2,'bipa222')
 
      IOP(2)=0
      IOP(9)=0
      IOP(10)=0
 
      DO 410 I=1,IOP(3)
      MARKP(I)=0
410   CONTINUE
      DO 420 I=1,IOP(5)
      MARKT(I)=0
420   CONTINUE
 
C --- treat the collisions between two nucleons
          DO 600 NUP=1,IOP(3)
C --- coordinates of projectile nucleon, (XP,YP)
C       with respect to the rest frame of the target center
      XP=CORP(NUP,1)+XPRO
      YP=CORP(NUP,2)+YPRO
      IF (XP.GT.XMAXT+CUTOFF) GOTO 600
      IF (XP.LT.XMINT-CUTOFF) GOTO 600
      IF (YP.GT.YMAXT+CUTOFF) GOTO 600
      IF (YP.LT.YMINT-CUTOFF) GOTO 600
 
          DO 500 NUT=1,IOP(5)
C --- distance between a projectile nucleon and a target nucleon
      R2=(XP-CORT(NUT,1))**2+(YP-CORT(NUT,2))**2
C --- judge if a binary collision takes place
 
      PP = FRVOV(R2)
 
      IF (RLU(0).LT.PP) THEN
         IOP(2)=IOP(2)+1
        IF (IOP(2).GT.3000) CALL FRMGOUT(0,0,'Array NUCT needs to be
     >  expanded (3000 insufficient)',0.,0.,0.,0.,0.)
         NUC(1,IOP(2))=NUP
         NUC(2,IOP(2))=NUT
       IF (MARKP(NUP).EQ.0) THEN
         MARKP(NUP)=1
         IOP(9)=IOP(9)+1
       ENDIF
       IF (MARKT(NUT).EQ.0) THEN
         MARKT(NUT)=1
         IOP(10)=IOP(10)+1
       ENDIF
      ENDIF
500   CONTINUE
600   CONTINUE
 
      IF (NREP.EQ.NUMREP) NFLG4=0
 
      IF ((KFR(3).EQ.1.OR.KFR(3).EQ.3).AND.IOP(9).LT.IOP(3)) GOTO 50
      IF (IOP(9).EQ.0) GOTO 50
 
C --- Order and pack the wounded nucleons in the front:
      DO 700 L=1, 2
      DO 670 I=1,IOP(3+2*(L-1))
        II=999
        DO 650 J=1,IOP(2)
650     IF (NUC(L,J).GE.I.AND.NUC(L,J).LT.II) II=NUC(L,J)
 
        IF (II.EQ.I) GOTO 670
        IF (II.EQ.999) GOTO 700
        DO 660 J=1,IOP(2)
        IF (NUC(L,J).EQ.II) THEN
          IDN(L,I)= IDN(L,II)
          FMN(L,I)= FMN(L,II)
          NUC(L,J)=I
        ENDIF
660     CONTINUE
670   CONTINUE
700   CONTINUE
 
 
      RETURN
      END
