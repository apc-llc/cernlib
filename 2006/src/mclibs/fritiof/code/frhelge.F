*
* $Id: frhelge.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frhelge.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRCOLPT **************************
 
 
C********************************* FRHELGE ******************************
 
      SUBROUTINE FRHELGE
 
C.... HELGE calculates the energy momenta for the nuclear spectator remnent
C.... and GIVES FERMI-MOTION TO THE NUCLEONS IN THE NUCLEI, here the input
C.... nucleons are assumed to be moving along the Z-axis.
C.... By giving the rest nucleons a fermi momentum, then binding energy
C.... has to be included to ensure energy-momentum conservation. This
C.... is achieved here by puting the nucleons off shell.
 
      PARAMETER (KSZ1=20, KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      DIMENSION FVECT(3,KSZ2)
      SAVE /FRINTN0/,/FRPARA1/,/FRINTN1/,/FRINTN3/
      DATA PI /3.1415926/
 
      DO 7 L=1,2
      NWD = IOP(3+2*(L-1))-IOP(8+L)
      DO 9 LO=1,4
  9      PPA(L,LO) = FLOAT(NWD)* PLI0(L,LO)
  7      PPA(L,5)=FRSQR(PPA(L,4)*PPA(L,3),'PPA')
 
      IF(KFR(4).EQ.0) RETURN
 
C......................................................................
      DO 700 L=1, 2
 
      IF (IOP(3+2*(L-1)).LE.1) GOTO 700
 
   15    DO 18 LO=1,5
   18    PPA(L,LO) = 0.0
 
       DO 10 I=1,3
 
        SUM=0.
        DO 20 J=1,IOP(3+2*(L-1))
          SL1=RLU(0)
          SL2=RLU(0)
          SL1=FRSQR(-2.*LOG(MAX(1.E-15,SL1))*IOP(3+2*(L-1))
     >                      /(IOP(3+2*(L-1))-1), 'SL1KL7')
          SL2=COS(2.*PI*SL2)
          FVECT(I,J)=SL1*SL2*.1
   20     SUM=SUM+FVECT(I,J)
 
        DO 12 J=1,IOP(3+2*(L-1))
   12     FVECT(I,J)=FVECT(I,J)-SUM/FLOAT(IOP(3+2*(L-1)))
 
   10 CONTINUE
 
      DO 30 J=1,IOP(3+2*(L-1))
        SUM2=0.
        DO 32 I=1,3
   32   SUM2=SUM2+FVECT(I,J)**2
        IF (SUM2.GE..3) GOTO 15
   30 CONTINUE
 
      DO 50 J=1,IOP(3+2*(L-1))
        PPS(L,J,1)=FVECT(1,J)
        PPS(L,J,2)=FVECT(2,J)
        BOSFAC=FRSQR(PPS(L,J,4)/PPS(L,J,3), 'BOS590')
C.......In the rest frame, keeping E=M unchanged,
C.......nucleon off shell, FMN(L,J) changed from nucleon rest masses.
      EE = PPS(L,J,5)
        PPLUS=EE+FVECT(3,J)
        PMINUS=EE-FVECT(3,J)
        EM2=PPLUS*PMINUS-FVECT(1,J)**2-FVECT(2,J)**2
 
      IF(EM2.LE.0.) GOTO 15
 
      PPS(L,J,5) = SQRT(EM2)
      FMN(L,J) = PPS(L,J,5)
        PPS(L,J,4)=PPLUS*BOSFAC
        PPS(L,J,3)=PMINUS/BOSFAC
      IF(J.GT.IOP(8+L)) THEN
      DO 52 LO=1,4
   52 PPA(L,LO)= PPA(L,LO)+ PPS(L,J,LO)
      ENDIF
       DO 55 LO=1,4
   55  PPSY(L,J,LO) = PPS(L,J,LO)
       PPSY(L,J,5) = FRSQR(PPSY(L,J,4)*PPSY(L,J,3)-PPSY(L,J,1)**2
     >               -PPSY(L,J,2)**2, 'PPSYHG')
   50  CONTINUE
 
       PPA(L,5)=FRSQR(PPA(L,4)*PPA(L,3)-PPA(L,1)**2-PPA(L,2)**2,'PA1')
 
 700   CONTINUE
 
      RETURN
      END
