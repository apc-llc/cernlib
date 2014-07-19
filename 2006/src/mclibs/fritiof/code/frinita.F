*
* $Id: frinita.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frinita.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C**************************** END FRITIOF *******************************
 
C**************************** FRINITA ************************************
 
      SUBROUTINE FRINITA(CFRAME,CBEAM,CTARG,WIN)
 
C Purpose: identifies the particles involved and fills common block
C Calculates initial momenta and fills common block FRINTN0.
C Write out information about the particles and the event selection.
C The program is stopped if the BEAM or TARGET particles or the frame
C are not recognized.
C This routine calls FRHILDN for setting the particle codes and masses.
 
      PARAMETER (KSZ1=20, KSZ2=300)
      CHARACTER CFRAME*4,CBEAM*4,CTARG*4, PARTIC*4,PACD*4
      CHARACTER INIT*42, CGDATE*11
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRCODES/IPT(2),PACD(27),NNUC(27),NPROT(27),KCD(27)
     >           ,RO1(27,2),EXMA(9,2)
      COMMON/FRGEOMC/NFLG,NUMROP,NUMROT,NUMREP
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
 
      SAVE VFR10
      SAVE /FRINTN0/,/FRINTN3/,/FRPARA1/,/FRCODES/,/FRGEOMC/,/LUDAT1/
      DATA VFR10 /0./
 
      DATA IQFST /0/
      DATA CGDATE /'14 NOV 1993'/
 
       IWR = 0
       IF(KFR(11).LT.0.OR.IQFST.LE.KFR(11)) IWR=1
C.....................Identify the particles involved....................
       DO 110 L=1, 2
       IPT(L) = 0
       IF(L.EQ.1) PARTIC=CBEAM
       IF(L.EQ.2) PARTIC=CTARG
       DO 100 J=1,27
       IF(PARTIC.EQ.PACD(J)) THEN
         IPT(L)=J
         IOP(3+2*(L-1))=NNUC(IPT(L))
         IOP(4+2*(L-1))=NPROT(IPT(L))
         IOP(6+L)=KCD(IPT(L))
         GOTO 110
       ENDIF
 100   CONTINUE
 110   CONTINUE
 
C................................................write headers........
       IF(MSTU(12).EQ.1) THEN
       WRITE(MSTU(11),1000) CGDATE
       CALL LULIST(0)
       WRITE(MSTU(11),*)
       WRITE(MSTU(11),*)
       ENDIF
 
       IF(IPT(1).EQ.0) WRITE(MSTU(11),1100) CBEAM
       IF(IPT(2).EQ.0) WRITE(MSTU(11),1200) CTARG
       IF(IPT(1).EQ.0.OR.IPT(2).EQ.0) STOP 99901
 
       IF(IOP(3).EQ.0.OR.IOP(5).EQ.0) THEN
        WRITE(MSTU(11),1300)
        STOP 99902
       ENDIF
       IF(IOP(3).GT.KSZ2) WRITE(MSTU(11),1310) IOP(3)
       IF(IOP(5).GT.KSZ2) WRITE(MSTU(11),1320) IOP(5)
       IF(IOP(3).GT.KSZ2.OR.IOP(5).GT.KSZ2) STOP 99903
 
C.....set minimum mass and minimum diffractive mass
 
      DO 112 L=1, 2
      IF(NNUC(IPT(L)).LE.1.and.IPT(L).LE.9) THEN
      AOP(8+L) = EXMA(IPT(L),1)
      AOP(10+L) = EXMA(IPT(L),2)
      ELSEIF(NNUC(IPT(L)).GE.2) THEN
      AOP(8+L) = EXMA(8,1)
      AOP(10+L) = EXMA(8,2)
      ENDIF
      IF(KCD(IPT(L)).NE.0) THEN
      PMAS = ULMASS(KCD(IPT(L)))
      IF(AOP(8+L).LT.PMAS) AOP(8+L)=PMAS
      ENDIF
      IF(AOP(8+L).LE.0.0001) AOP(8+L) = EXMA(8,1)
      IF(AOP(10+L).LT.AOP(8+L)) AOP(10+L) = AOP(8+L)
112   CONTINUE
 
      CALL FRHILDN
 
       DO 115 L=1,2
       DO 115 LO=1,2
115    PLI0(L,LO) = 0.
 
       IF(CFRAME.EQ.'CMS') THEN
         INIT=CBEAM//'-'//CTARG//' COLLIDER'//' '
         IF(IWR.EQ.1) WRITE(MSTU(11),1400) INIT
         IF(IWR.EQ.1) WRITE(MSTU(11),1500) WIN
 
C...IN CASE OF NUCLEUS, HERE WE HAVE NEGLECTED THE MASS DIFFERECE
C...BETWEEN THE NUCLEONS.........................................
         S0=WIN**2
         FP = FMN(1,1)**2
         FT = FMN(2,1)**2
         PL2=S0/4-(FP+FT)/2+(FP-FT)**2/(4*S0)
         ESEN=FRSQR(PL2 + FP, 'ESENPL' )
         PLI0(1,4) =ESEN+FRSQR(PL2, 'euiron' )
         PLI0(1,3) =FP/PLI0(1,4)
 
         ESEN=FRSQR(PL2 + FT, 'ESENFT')
         PLI0(2,3) =ESEN+FRSQR(PL2,'iopji1' )
         PLI0(2,4) =FT/PLI0(2,3)
       AOP(1) = WIN
 
       ELSEIF(CFRAME.EQ.'FIXT') THEN
 
       PLAB = WIN
       ELAB = SQRT(PLAB**2+FMN(1,1)**2)
          PLI0(1,4) =ELAB+PLAB
          PLI0(1,3) =FMN(1,1)**2/PLI0(1,4)
          PLI0(2,4) =FMN(2,1)
          PLI0(2,3) =FMN(2,1)
      S0 = (PLI0(1,4)+PLI0(2,4))*(PLI0(1,3)+PLI0(2,3))
        AOP(1) = SQRT(S0)
         INIT=CBEAM//' ON '//CTARG//' FIXED TARGET'//' '
         IF(IWR.EQ.1) THEN
       WRITE(MSTU(11),1400) INIT
         IF(IOP(3).GT.1) WRITE(MSTU(11),1600) WIN, AOP(1)
         IF(IOP(3).EQ.1) WRITE(MSTU(11),1610) WIN, AOP(1)
       ENDIF
       ELSE
         WRITE(MSTU(11),2000) CFRAME
       STOP 99904
       ENDIF
 
       IF(IWR.EQ.1) THEN
       WRITE(MSTU(11),2005) IOP(3),IOP(4),IOP(5),IOP(6)
       IF(IOP(3)+IOP(5).GT.2) WRITE(MSTU(11),2007)
       ENDIF
 
 
C.....Evaluate cross sections :
 
       IF(NFR(1).GT.0.AND.ABS(VFR(10)-VFR10).LT.0.001) THEN
       VFR(10) = 0.
       VFR(11) = 0.
       ENDIF
 
       CALL FRQPROB(IDN(1,1),IDN(2,1),IWR)
       VFR10 = VFR(10)
 
C..... Set up a few control parameters for the geometry package:
C ...  NFLG is the entry control of subroutines FRPACOL & FRAACOL
 
       NFLG=0
       NUMROP=1
       if (IOP(3).gt.25) NUMROP=3
       NUMROT=1
       if (IOP(5).gt.25) NUMROT=3
       if (KFR(6).eq.1.or.(KFR(6).eq.2.and.IOP(5).gt.79) ) NUMROT=200
       NUMREP=1
       if (IOP(3).gt. 50.or.IOP(5).gt. 50) NUMREP=3
       if (IOP(3).gt.100.or.IOP(5).gt.100) NUMREP=6
       if (IOP(3).gt.200.or.IOP(5).gt.200) NUMREP=10
 
 
      IF(NFR(1).EQ.0) THEN
       IF(KFR(2).EQ.0) THEN
        WRITE(MSTU(11),2100)
       ELSEIF(KFR(2).EQ.1) THEN
        WRITE(MSTU(11),2110)
        WRITE(MSTU(11),2120) VFR(8),VFR(9)
       ENDIF
       IF(KFR(1).EQ.0) THEN
        WRITE(MSTU(11),2130)
       ELSEIF(KFR(1).EQ.1) THEN
        WRITE(MSTU(11),2140)
       ENDIF
       IF(IOP(5).GT.1) THEN
        IF(KFR(3).EQ.0) THEN
         WRITE(MSTU(11),2200)
        ELSEIF(KFR(3).EQ.1.or.KFR(3).EQ.3) THEN
         WRITE(MSTU(11),2210)
        ELSEIF(KFR(3).EQ.2.or.KFR(3).EQ.3) THEN
         WRITE(MSTU(11),2220) VFR(1),VFR(2)
        ENDIF
        IF(KFR(4).EQ.0) THEN
         WRITE(MSTU(11),2230)
        ELSEIF(KFR(4).EQ.1) THEN
         WRITE(MSTU(11),2240)
        ENDIF
        IF(KFR(5).EQ.0) THEN
         WRITE(MSTU(11),2250)
        ELSEIF(KFR(5).EQ.1) THEN
         WRITE(MSTU(11),2254)
        ELSEIF(KFR(5).EQ.2) THEN
         WRITE(MSTU(11),2258)
        ENDIF
        IF(KFR(6).EQ.0.or.(KFR(6).eq.2.and.IOP(5).le.79)) THEN
         WRITE(MSTU(11),2260)
        ELSE
         WRITE(MSTU(11),2270) VFR(4), VFR(5)
        ENDIF
       ENDIF
       IF(KFR(7).eq.0) THEN
        WRITE(MSTU(11),2280)
       ELSEIF(KFR(7).eq.1) THEN
        WRITE(MSTU(11),2290)
       ELSEIF(KFR(7).eq.2) THEN
        WRITE(MSTU(11),3000)
       ENDIF
       WRITE(MSTU(11),*) '  '
      ENDIF
 
       IQFST= IQFST+1
 
C........................FORMATS FOR INITIALIZATION AND ERROR INFORMATION
 1000 FORMAT(//20X,'THE LUND MONTE CARLO - FRITIOF VERSION 7.02'/
     *         20X,'LAST DATE OF CHANGE/BUG FIXING: ', A11)
 1100 FORMAT(1X,'ERROR: UNRECOGNIZED BEAM PARTICLE ''',A,
     * '''. EXECUTION STOPPED.')
 1200 FORMAT(1X,'ERROR: UNRECOGNIZED TARGET PARTICLE ''',A,
     * '''. EXECUTION STOPPED.')
 1300 FORMAT(1X,'ERROR: PARTICLES NOT WELL DEFINED. EXECUTION STOPPED.')
 1310 FORMAT(1X,'ERROR: TOO LARGE PROJECTILE, IOP(3)= ',I5,
     * '. EXECUTION STOPPED.')
 1320 FORMAT(1X,'ERROR: TOO LARGE TARGET,IOP(5)= ',I5,
     * '. EXECUTION STOPPED.')
 1400 FORMAT(/1X,77('=')/1X,'|',75X,'|'/1X,'|',8X,'FRITIOF WILL BE ',
     *'INITIALIZED FOR',1X,A34,1X,'|')
 1500 FORMAT(1X,'|',15X,'AT',1X,F10.3,1X,'GEV CENTER-OF-MASS ENERGY',
     *21X,'|'/1X,'|',75X,'|'/1X,77('='))
 1600 FORMAT(1X,'|',12X,'AT',1X,F10.3,1X,'GEV/C LAB-MOMENTUM PER NUCLEON
     *',19X,'|'/1x,'|',18X,'Equivalent CMS energy W= ',F9.4,1X,'GeV',
     > 19X,'|'/1X,'|',75X,'|'/1X,77('='))
 1610 FORMAT(1X,'|',23X,'AT',1X,F10.3,1X,'GEV/C LAB-MOMENTUM
     *',8X,'|'/1x,'|',18X,'Equivalent CMS energy W= ',F9.4,1X,'GeV',
     > 19X,'|'/1X,'|',75X,'|'/1X,77('='))
 2000 FORMAT(1X,'ERROR: UNRECOGNIZED COORDINATE FRAME ''',A,
     *'''. EXECUTION STOPPED.')
 2005 FORMAT(1X,5X,'PROJECTILE (A,Z)= ','(',I3,',',I3,')',
     > 4X,'TARGET (A,Z)= ','(',I3,',',I3,')',/)
 2007 FORMAT(/,/,4X,'A REMINDER: if the event is listed by LULIST,',
     > 1x,'the lines without' /,4x,'character names represent the',
     > 1x,'spectator nuclei.')
2100   FORMAT(/,/,4X,'No gluon radiation')
2110   FORMAT(    4X,'Gluon radiation included')
2120   FORMAT(4X,'Mu (projectile) =',F5.2,1x,'GeV;',3x,
     >           'Mu (target) =',F5.2,1x,'GeV')
2130   FORMAT(    4X,'Fragmentation not performed')
2140   FORMAT(    4X,'Fragmentation performed')
2200   FORMAT(    4X,'All interactions recorded')
2210   FORMAT(    4X,'Spectator veto')
2220   FORMAT(4X,'Impact parameter restricted in',
     >                                 1x,f7.3,'-',f7.3,' fm')
2230   FORMAT(    4X,'NO Fermi motion')
2240   FORMAT(    4X,'Fermi motion included')
2250   FORMAT(    4X,'Overlap function: eikonal')
2254   FORMAT(    4X,'Overlap function: gaussian')
2258   FORMAT(    4X,'Overlap function: gray disc')
2260   FORMAT(    4X,'No target nucleus deformation')
2270   FORMAT(    4X,'Target nucleus deformation applied.',1x,
     >            'Dipole and quadrupole coeff: ',f7.3,',',2x,f7.3)
2280   FORMAT(    4X,'No QCD parton scattering')
2290   FORMAT(    4X,'Hardest Rutherford parton scattering included')
3000   FORMAT(    4X,'Multiple parton scattering included')
 
      RETURN
      END
