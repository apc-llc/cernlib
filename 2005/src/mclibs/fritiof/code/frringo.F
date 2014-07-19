*
* $Id: frringo.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frringo.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C*************************************************************************
 
C******************* END OF PACKAGE FOR NUCLEAR GEOMETRY *****************
 
 
 
 
C*************************************************************************
C                                                                        *
C This is the routine package for generating nucleon-nucleon collisions  *
C                                                                        *
C*************************************************************************
 
C********************************* FRRINGO *******************************
 
      SUBROUTINE FRRINGO
 
C ....................    THE ROUTINE GIVES MASSES TO THE EXCITED NUCLEONS
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZ1=20,KSZ2=300,KSZJ=4000)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRINTN2/NHP(2),IHQP(2,KSZ2),KHP(2,KSZ2,100,5),
     >   PHP(2,KSZ2,100,5)
      COMMON/FRCNUT/NR,KR(10,5),PR(10,5),NR0
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
 
      DIMENSION DPTSQ(2),DP(2,5),DN(2,5),DSM(2,5),DSYM(2,5),DBETA(3),
     > PJK(2,4),PPSYU(2,5),FMN0(2),RM(2,2)
      SAVE PXGAU,PYGAU,RM
      SAVE /FRINTN0/,/FRPARA1/,/FRINTN3/,/FRINTN1/,/FRINTN2/,/FRCNUT/,
     >     /FRJETS/,/PYPARS/,/LUJETS/,/PYSUBS/
      DATA RM /0.,0.,0.,0./
 
      CALL FRDOICT(-1)
 
C..Monitors ICT(10) (for current event) and ICTT (for all events) are set up:
C...   ICT(kfel) counts the number of times error KFEL occurs.
C...   ICT(7) is the number of ``single diff'' collisions.
C...   ICT(8) the number of collisions bypassed due to repeated errors.
C...  KFEL=2,3,8,10 result in the collision being skipped.
C...  KFEL used: 1-10
 
C...........................................set initial momenta
 
10    KFEL=0
      DO 20 L=1,2
      DO 20 I=1,IOP(3+2*(L-1))
        DO 21 LO=1,4
   21   PPS(L,I,LO)= PLI0(L,LO)
        PPS(L,I,5)= FRSQR(PPS(L,I,4)*PPS(L,I,3)-PPS(L,I,1)**2
     >                    -PPS(L,I,2)**2, 'reyu90')
        IHQP(L,I) = 0
        kHP(L,I,1,5)=0
        PHP(L,I,1,5)=-1.
        PHP(L,I,2,5)=-1.
        DO 20 LO=1,5
        PPH(L,I,LO) = 0.
   20   PPSY(L,I,LO) = PPS(L,I,LO)
 
C....Spectator momenta and FERMI-MOTION  ............
 
      CALL FRHELGE
 
C.....loop over all binary collisions
 
      IOP(13) = 0
      IOP(14) = 0
      IOP(15) =0
      NHP(1) = 0
      NHP(2) = 0
      NR = 0
      IF(IOP(2).GT.3000) CALL FRMGOUT(0,0,
     >  '** NUC(2,3000) must be increased **',0.,0.,0.,0.,0.)
 
      DO 1000 I=1,IOP(2)
 
      IOP(1) = I
      NU1 = NUC(1,I)
      NU2 = NUC(2,I)
      KFELS=0
 
C.....FRVECTC SETS FOUR-VECTORS DP(2,4) TO PPS .....
      CALL FRVECTC(I, 1, DP)
      CALL FRVECTC(I, 1, DSM)
      CALL FRVECTC(I, 2, DSYM)
 
C...............................TOTAL E-P BEFORE THE COLLISION
 
      DO 30 LO = 1, 4
30    PPSYU(1,LO) = PPSY(1,NU1,LO)+PPSY(2,NU2,LO)
 
C.....BOOST to remnent-remnent CMS ......................
 
      CALL FRTOCMS(1, 1, DP, DBETA)
 
C....Skip the collision when strings moving backwards in CMS:
      IF(DP(1,4)-DP(1,3).LE.0.D0) THEN
C     KFEL=10
      CALL FRDOICT(10)
      GOTO 1000
      ENDIF
 
C.... ANGLES OF THE MOMENTUM VECTORS ................................
 
      CALL FRPOLAR(DTHE,DPHI, DP)
 
C ...  ROTATE SO P GOES TO THE Z-AXE ...................
 
      CALL FRROTAR(DTHE,DPHI,1, DP)
C....
      NR0 = NR+1
 
      SMP= REAL( DP(1,3)*DP(1,4)-DP(1,1)**2-DP(1,2)**2)
      SMT= REAL( DP(2,3)*DP(2,4)-DP(2,1)**2-DP(2,2)**2)
 
      W=REAL( DP(1,4)+DP(2,4) )
      PK2= FRKVM(W,SMP,SMT)
 
50    IF(IOP(15).EQ.0) THEN
      DO 60 LL=1,2
      DO 60 LO=1,4
60    PJK(LL,LO) = 0.
      ENDIF
 
C............................................GENERATE HARD PARTONS.......
 
      IHAV = 0
      NJ = 0
      IF(IOP(18).GE.1.AND.IOP(15).EQ.0) then
 
      FMN0(1) = ULMASS(IDN(1,NU1))
      FMN0(2) = ULMASS(IDN(2,NU2))
C...................................effective energy for hard scattering
      P13 = FMN0(1)**2/REAL(DP(1,4))
      P24 = FMN0(2)**2/REAL(DP(2,3))
      E1 = 0.5*( REAL(DP(1,4))+P13 )
      P1 = 0.5*( REAL(DP(1,4))-P13 )
      E2 = 0.5*( REAL(DP(2,3))+P24 )
      P2 = 0.5*(-REAL(DP(2,3))+P24 )
 
      CKIN(22)=MIN(1.,REAL(DP(1,3))/P13)
      CKIN(24)=MIN(1.,REAL(DP(2,4))/P24)
 
      WEF =FRSQR(FMN0(1)**2+FMN0(2)**2+2.*(E1*E2-P1*P2),' WEFIS1')
      IF(WEF.LT.PARP(2)) GOTO 495
 
      N=0
 
      P(1,1) = 0.
      P(1,2) = 0.
      P(2,1) = 0.
      P(2,2) = 0.
      P(1,3) = P1
      P(2,3) = P2
      P(1,4) = E1
      P(2,4) = E2
 
C.....PYTHIA is reinitialized whenver the collision enviroment is changed.
C.....if momenta unchanged, no need to reinitialize PYTHIA, save time.....
      INI = 2
      DO 216 LO=1,2
      DO 216 J=3,4
216   IF( ABS(P(LO,J)-RM(LO,J-2)).GT.0.01*RM(LO,J-2) ) INI=1
 
      IF(INI.EQ.1) THEN
      DO 218 LO=1,2
      DO 218 J=1,2
218   RM(LO,J) = P(LO,J+2)
C.....reset parameters before every new collision:
      CALL FRSETPY(1)
      ENDIF
 
C.....PYTHIA is reinitialized here only when the collision energy changes, but
C.....not when the particle changes (neutron instead of a proton, for exemple).
 
      NFR(2) = NFR(2)+1
      CALL FRHARDP(IDN(1,NU1),IDN(2,NU2),Wef,IHAV,INI)
      IF(IHAV.EQ.0)     GOTO 495
 
      DO 220 J=1, NJ
      PJK(ABS(KJ(J,3)),1) = PJK(ABS(KJ(J,3)),1)+ PJ(J,1)
      PJK(ABS(KJ(J,3)),2) = PJK(ABS(KJ(J,3)),2)+ PJ(J,2)
      PJK(ABS(KJ(J,3)),3) = PJK(ABS(KJ(J,3)),3)+ PJ(J,4) - PJ(J,3)
220   PJK(ABS(KJ(J,3)),4) = PJK(ABS(KJ(J,3)),4)+ PJ(J,4) + PJ(J,3)
 
      ENDIF
C........................................................................
 
 
C..........generate soft PT in REMNENT-REMNENT CMS frame ......
 
495   ICPK=0
      Irep = 0
 
      AHT=0.
      IF(IOP(15).GE.2.AND.KFR(9).EQ.1) AHT=1.0
 
600   PXGAU = 0.
      PYGAU = 0.
 
      IF(ICPK.LE.100) THEN
      PX0 = AHT*PJK(1,1)
      PY0 = AHT*PJK(1,2)
      CALL FRCOLPT(PK2,PXGAU,PYGAU,PX0,PY0)
      ICPK = ICPK+1
        IF(IOP(15).EQ.0.AND.NJ.GT.0) THEN
        PTTRY = (PXGAU+PJK(1,1))**2+ (PYGAU+PJK(1,2))**2
        IF(PTTRY.GT.PK2) GOTO 600
        ENDIF
      ENDIF
 
500   DN(1,1) = DBLE(PXGAU)
      DN(1,2) = DBLE(PYGAU)
      DN(2,1) = -DBLE(PXGAU)
      DN(2,2) = -DBLE(PYGAU)
 
      DPTSQ(1) = DN(1,1)**2 + DN(1,2)**2
      DPTSQ(2) = DN(2,1)**2 + DN(2,2)**2
 
C.............................................................
 
      IF(KFEL.GT.0) THEN
      KFELS=KFELS+1
      CALL FRDOICT(KFEL)
      ENDIF
 
C  Bypass the collision if too many errors:
      IF (KFELS.GT.20) THEN
      IF(I.EQ.1) GOTO 10
      KFEL=8
      GOTO 999
      ENDIF
 
C............... SOFT MOMENTUM TRANSFERS ...........................
 
610   CALL FRPSOFT(I,PJK,DP,DN,KFEL)
      IF(KFEL.GT.0) GOTO 999
 
C.....excited masses for soft remnents
      DWI =DN(1,4)*DN(1,3)
      DWT =DN(2,4)*DN(2,3)
      IF(DWI.LT.DPTSQ(1).OR.DWT.LT.DPTSQ(2)) THEN
      kfel=1
      GOTO 500
      ENDIF
 
      DN(1,5)=DFRSQR(DWI-DPTSQ(1), 'DWI123')
      DN(2,5)=DFRSQR(DWT-DPTSQ(2), 'DNW935')
 
 
C.....transform DN & PJ() & PR() back to original frame
 
      IF(IHAV.EQ.1.AND.IREP.EQ.0) THEN
      IQL = -2
      ELSE
      IQL = -1
      ENDIF
 
       CALL FRROTAR(DTHE,DPHI,IQL, DN)
       CALL FRTOCMS(1, IQL, DN, DBETA)
       Irep = 1
 
C......... UPDATE THE PPS(1-2,I,4) ARRAY:
      CALL FRVECTC(I, -1, DN)
 
      PPS(1,NU1,5) =REAL(DN(1,5))
      PPS(2,NU2,5) =REAL(DN(2,5))
 
C..................................update PPSY ..........
      CALL FRFILHD(I,0,KFEL)
      IF(KFEL.GT.0) GOTO 500
 
C.....treatment of diffractive collision
 
      CALL FRSETDM(I,KFEL)
C     ...single diffractive
      IF(KFEL.EQ.-1) CALL FRDOICT(7)
      IF(KFEL.GE.5)  GOTO 500
 
C....................STORE THE HARD PARTONS TO FRINTN2 ..........
 
      IF(IOP(15).EQ.0) CALL FRFILHD(I,1,KFEL)
 
C...TO test the Rutherford scattering against the background:
 
       N=0
       IF(NJ.GT.0.AND.IOP(15).EQ.0)
     >       CALL FRCHEXG(*50,I)
 
       IF(IOP(15).LE.1.AND.NJ.GT.0) THEN
         IOP(13) = IOP(13) + 1
         IOP(14) = IOP(14) + (NJ+1)/2
       ENDIF
 
 990   KFEL=0
       IOP(15)=0
       NJ=0
 
C.........CHECK E-P CONSERVATION .....................
      DO 910 LO = 1,4
910   PPSYU(2,LO) = PPSY(1,NU1,LO)+PPSY(2,NU2,LO)
 
      DO 920 LO = 4, 1, -1
      PERR = ABS(PPSYU(2,LO)-PPSYU(1,LO))
      IF(PERR.GE.MAX(0.1+0.5*(LO/3),0.05*PPSYU(1,LO)) ) THEN
      CALL FRMGOUT(1,1,'FRRINGO E-P not conserved:',
     >  PPSYU(2,1)-PPSYU(1,1),PPSYU(2,2)-PPSYU(1,2),
     >  PPSYU(2,3)-PPSYU(1,3),PPSYU(2,4)-PPSYU(1,4),PPSYU(1,4))
      GOTO 1000
      ENDIF
920   CONTINUE
 
      GOTO 1000
999   CALL FRVECTC(I, -1, DSM)
      CALL FRVECTC(I, -2, DSYM)
      CALL FRDOICT(KFEL)
1000  CONTINUE
 
      RETURN
      END
