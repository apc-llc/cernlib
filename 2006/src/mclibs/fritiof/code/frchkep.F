*
* $Id: frchkep.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frchkep.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C*********************************************************************
C********************************************************************
 
      SUBROUTINE FRCHKEP(IQ)
 
C... CHECK THE TOTAL CHARGE, ENERGY, MOMENTUM CONSERVATION
C... IQ=0: Just check the sums and take no further steps
C...   =1: Monitor the number of errors and signal (IOP(16)) for printout
C...       via FRMGOUT.
 
      PARAMETER (KSZJ=4000,KSZ1=20, KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
 
      Data ifst /0/
      Save Etot, Ptot, Cgtot
      SAVE /FRINTN0/,/LUJETS/,/LUDAT1/
 
      if(Ifst.eq.0) then
C  TOTAL Beam energy, momentum:
      EBM = 0.5*(PLI0(1,4)+PLI0(1,3))* IOP(3)
      PBM = 0.5*(PLI0(1,4)-PLI0(1,3))* IOP(3)
C  TOTAL target energy, momentum:
      ETG = 0.5*(PLI0(2,4)+PLI0(2,3))* IOP(5)
      PTG = 0.5*(PLI0(2,4)-PLI0(2,3))* IOP(5)
 
      Etot = EBM + ETG
      Ptot = PBM + PTG
      Cgtot = IOP(4)+ IOP(6)
      Ifst=1
      endif
 
      charg =0.0
      EE=0.0
      PPz=0.0
      PPx=0.0
      PPy=0.0
      DO 100 J=1, N
      IF(ABS(K(J,2)).GE.10000) then
        charg = charg+ ABS(K(J,2))-10000
        EE = EE+ P(j,4)
        PPz = PPz+ P(j,3)      
        PPy = PPy+ P(j,2)      
        PPx = PPx+ P(j,1)
      elseif(K(J,1).ge.1.and.K(J,1).le.5) then
        charg = charg+ PLU(j,6)
        EE = EE+ P(j,4)
        PPz = PPz+ P(j,3)      
        PPy = PPy+ P(j,2)      
        PPx = PPx+ P(j,1)
      endif
100      CONTINUE
 
      Ifel=0
      if(abs(PPx).gt.0.5.or.abs(PPy).gt.0.5) Ifel =1
      if(abs(PPZ-Ptot).gt.MAX(0.01*Ptot,0.5)) Ifel =1
      if(abs(EE-Etot).gt.MAX(0.01*Etot,0.5)) Ifel =1
      if(abs(charg-Cgtot).gt.0.01) Ifel =1
      
      if(ifel.eq.1) then
      write(MSTU(11), 1000)  NFR(1)
        IF(IQ.EQ.1)
     >     CALL FRMGOUT(-1,0,'Charge or energy non-conservation',
     >                   0.,0.,0.,0.,0.)
      write(MSTU(11), 1010) Ptot, Etot, Cgtot
      write(MSTU(11), 1020) PPx,PPy, PPz, EE, charg
      endif
 
1000      format( /,'???????????????????????????????????????????'
     >      /,'  Charge or energy non-conservation at event:', I6 )
1010      format('  Original Pz, E, Charge: ', 26x, 2E13.4, F6.1)
1020      format(' Total Px, Py, Pz, E, Cg: ', 4E13.4, F6.1 )
      RETURN
      END      
