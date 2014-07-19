*
* $Id: frsetdm.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frsetdm.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRFILHD **************************
 
C********************************* FRSETDM ******************************
 
      SUBROUTINE FRSETDM(I,KFEL)
 
C...TO RESET THE MASS IF FOUND DIFFRACTIVE, M<AOP(10+L).
C...I is the index for the collision
C...kfel = 0: not a "diffractive" event, no reset necessary
C....... = 5: fail to properly reset, regenerate the event may be needed.
C....... = 6: double diffractive
C....... = -1: reset successfully
 
      IMPLICIT DOUBLE PRECISION (D)
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      DIMENSION PTSQ(2),PTSQS(2),PNEW(2)
      SAVE /FRINTN0/,/FRINTN3/,/FRINTN1/
 
      IF(PPSY(1,NUC(1,I),5).LT.AOP(11).and.
     >                 PPSY(2,NUC(2,I),5).LT.AOP(12)) THEN
C      "double diffractive"
      KFEL=6
      RETURN
      ELSEIF(PPSY(1,NUC(1,I),5).GE.AOP(11).and.
     >                 PPSY(2,NUC(2,I),5).GE.AOP(12)) THEN
      KFEL=0
      RETURN
      ELSEIF(PPSY(1,NUC(1,I),5).LT.AOP(11)) THEN
      KFEL=-1
             L = 1
      ELSEIF(PPSY(2,NUC(2,I),5).LT.AOP(12)) THEN
      KFEL=-1
             L = 2
      ENDIF
 
C-------RESET THE MASS -------------------------------------------------
C.......Keep Pt fixed and choose P_large between pmin and pmax acc dP/P,
C.......which is equivalent to a uniform distributn of y.
C.......(A bad choice here may result in an ugly peak in dn/dy_proton)
 
      NV = NUC(L,I)
      NVV = NUC(3-L,I)
      FM = ULMASS(IDN(L,NUC(L,I)))
      LG=4
      IF( PPSY(L,NV,3).GT.PPSY(L,NV,4) ) LG=3
 
      PTSQ(L) = PPSY(L,NV,1)**2+PPSY(L,NV,2)**2
      PTSQS(L) = PPS(L,NV,1)**2+PPS(L,NV,2)**2
      TMP20 = FM**2 + PTSQ(L)
      DPMAX = PPSY(L,NV,LG)
      DPMIN = TMP20/PPSY(L,NV,7-LG)
         PNEW(LG-2) = DFRDPQ(DPMIN,DPMAX,0.)
         PNEW(5-LG) = TMP20/PNEW(LG-2)
      ADELP = PNEW(2) - PPSY(L,NV,4)
      ADELM = PNEW(1) - PPSY(L,NV,3)
      PPSY(L,NV,4) = PNEW(2)
      PPSY(L,NV,3) = PNEW(1)
      PPS(L,NV,4) = PPS(L,NV,4)  + ADELP
      PPS(L,NV,3) = PPS(L,NV,3)  + ADELM
      RMS34S = PPS(L,NV,3)*PPS(L,NV,4)
          IF(RMS34S.LE.PTSQS(L)) GOTO 500
      PPSY(L,NV,5) =FM
      PPS(L,NV,5) =FRSQR(RMS34S-PTSQS(L),'ppslnv5')
 
C   Rebalance the energy-momentum
 
      PTSQ(3-L) = PPSY(3-L,NVV,1)**2+PPSY(3-L,NVV,2)**2
      PTSQS(3-L) = PPS(3-L,NVV,1)**2+PPS(3-L,NVV,2)**2
      PPSY(3-L,NVV,4) = PPSY(3-L,NVV,4) - ADELP
      PPSY(3-L,NVV,3) = PPSY(3-L,NVV,3) - ADELM
      PPS(3-L,NVV,4) = PPS(3-L,NVV,4) - ADELP
      PPS(3-L,NVV,3) = PPS(3-L,NVV,3) - ADELM
      RMSPT = AOP(12-L+1)**2 + PTSQ(3-L)
      RMS34 = PPSY(3-L,NVV,4)*PPSY(3-L,NVV,3)
      RMS34S = PPS(3-L,NVV,4)*PPS(3-L,NVV,3)
          IF(RMS34.LT.RMSPT.OR.RMS34S.LE.PTSQS(3-L)) GOTO 500
      PPSY(3-L,NVV,5)=FRSQR(RMS34-PTSQ(3-L), 'DN25IS')
      PPS(3-L,NVV,5)=FRSQR(RMS34S-PTSQS(3-L), 'DN25SS')
 
      GOTO 600
500   KFEL=5
600   RETURN
      END
