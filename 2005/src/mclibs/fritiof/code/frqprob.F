*
* $Id: frqprob.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frqprob.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRORD01 **************************
 
 
 
C*************************************************************************
C**                                                                     **
C**   This package interfaces with PYTHIA and handles the generated     **
C**   hard partons                                                      **
C**                                                                     **
C*************************************************************************
 
C********************************* FRQPROB ******************************
 
      SUBROUTINE FRQPROB(KFI,KFT,IQ)
 
C.... TO ESTIMATE cross sections.
C......KFI, KFT - The KF codes of the incident and target particle (nucleon).
C......IQ=0 will suppress all the write out.
 
 
      PARAMETER (KSZJ=4000,KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRCODES/IPT(2),PACD(27),NNUC(27),NPROT(27),KCD(27)
     >           ,RO1(27,2),EXMA(9,2)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      CHARACTER PACD*4,NAM1*16,NAM2*16,NAM1S*8,NAM2S*8,WORDS*21
      SAVE /FRPARA1/,/FRCODES/,/FRINTN0/,/LUJETS/,/LUDAT1/,/PYSUBS/,
     >     /PYPARS/,/PYINT1/,/PYINT5/
 
      N=0
      DO 100 L = 1,2
       DO 120 J = 1, 2
120    P(L,J) = PLI0(L,J)
      P(L,3) = (PLI0(L,4)-PLI0(L,3))/2.
100   P(L,4) = (PLI0(L,4)+PLI0(L,3))/2.
 
      W = AOP(1)
 
      IF(W.LE.PARP(2)) THEN
        IF(KFR(7).GT.0) WRITE(MSTU(11),3000) W
        KFR(7)= 0
        PARP(2)= 0.9*W
      ENDIF
      IOP(18)= KFR(7)
 
      CALL LUNAME(KFI,NAM1)
      CALL LUNAME(KFT,NAM2)
      NAM1S = NAM1
      NAM2S = NAM2
 
C.....................................
      CALL FRSETPY(1)
      CALL FRHARDP(KFI, KFT, W, IHAV,-1)
 
Cc      XQCD = XSEC(11,3)+XSEC(12,3)+XSEC(13,3)+
Cc     >         XSEC(28,3)+XSEC(53,3)+XSEC(68,3)+
Cc     >         XSEC(81,3)+XSEC(82,3)+XSEC(83,3)
 
C......VINT(103) = SIGL DIFF CROSS SECTION; VINT(106) = NON-DIFF INELASTIC
C......If target is a nuclei, xsections are taken as the average of N,P:
 
990   IF(IQ.GT.0) WRITE(MSTU(11),999)
999   FORMAT(/1x,79('-')/
     >   4x,'FRITIOF-FRQPROB reporting: ',/)
 
      XINEL = VINT(106) + VINT(103)
      XTOT = VINT(101)
      XEL = VINT(102)
 
      IF(NNUC(IPT(2)).GT.1) THEN
      CALL FRHARDP(KFI, 4324-IABS(KFT), W, IHAV,-1)
      XTOT = (XTOT+VINT(101))/2.
      XEL = (XEL+VINT(102))/2.
      XINEL = (XINEL+VINT(106)+VINT(103))/2.
      NAM2 = 'Nucleon'//' '
      ENDIF
 
      WORDS = '(from the input)'
      IF(VFR(10).LE.0.OR.VFR(11).LE.0.OR.IQ.GT.0)THEN
       IF(VFR(10).LE.0.) VFR(10) = XTOT
       IF(VFR(11).LE.0.) VFR(11) = XEL
       words = '(from Block-Cahn fit)'
      ENDIF
 
       IF(IQ.GT.0) THEN
       WRITE(MSTU(11),2100)
     >       NAM1S,NAM2S,WORDS, VFR(10),VFR(11),XINEL
        WRITE(MSTU(11),3001)
       ENDIF
 
2100  FORMAT(6X,'Cross sections for ',A8,'-- ',A8,' are',1X,A21,':',/
     >        8x,'Total cross section=', F10.3, ' mb',/
     >        8x,'Elastic cross section=', F10.3, ' mb',/
     >   6X,'Non-double diffractive inelastic xsection= ',F8.3,' mb')
3000  FORMAT(/4x,'Warning! W_CMS=',F6.2,'-- W too small for ',
     > 'hard scattering!',
     > /4x,'Excecution continues with RPS switched off!',
     > /4x,'(Please refer to the PYTHIA parameter PARP(2))',/ )
3001  FORMAT(1x,79('-')/)
 
      RETURN
      END
