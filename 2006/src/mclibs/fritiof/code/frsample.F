*
* $Id: frsample.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frsample.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C******************************************************************************
C** This file has LUJETS size KSZJ=4000.
C******************************************************************************
C******************************************************************************
C**--------------------------------------------------------------------------**
C**                                                                          **
C**                                                 June, 1992               **
C**                                     Modified: Nov.14, 1993               **
C**                                                                          **
C**                                                                          **
C**           Lund Monte Carlo for hadron-hadron, hadron-nucleus             **
C**                                                                          **
C**                     and nucleus-nucleus collisions                       **
C**                                                                          **
C**                                                                          **
C**                            FRITIOF version 7.02                          **
C**                                                                          **
C**                                                                          **
C**    AUTHORS:                                                              **
C**    FRITIOF 7.0:       HONG PI                                            **
C**    FRITIOF 4.0 - 6.0: LINKAI DING, EVERT STENLUND, ULF PETTERSSON        **
C**    FRITIOF 3.1: BO NILSSON-ALMQVIST, EVERT STENLUND, CONNY SJ\"OGREN     **
C**    FRITIOF 1.6 - 1.7: BO NILSSON-ALMQVIST, EVERT STENLUND                **
C**                                                                          **
C**          Department of Theoretical Physics, University of Lund           **
C**                 Solvegatan 14a, S-223 62 Lund, Sweden                    **
C**                                                                          **
C**                                                                          **
C**              * PLEASE REPORT PROGRAMME ERRORS TO AUTHORS *               **
C**                                                                          **
C**                INTERNET: pihong@thep.lu.se                               **
C**                BITNET: THEPHP@SELDC52.BITNET                             **
C**                                                                          **
C**                                                                          **
C**    Notice:                                                               **
C**    Fritiof 7.02 subroutine packages must be compiled together with       **
C**    ARIADNE 4.02r, JETSET 7.3, and PYTHIA 5.5.  Be sure you have the      **
C**    proper versions of these LUND programs.                               **
C**                                                                          **
C**--------------------------------------------------------------------------**
C******************************************************************************
C**   Draft code: 2931114                                                     |
C**||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
C-----------------------------------------------------------------------------
C... This is a sample main program - disguised here as a subroutine,
C... which can be used to test the installation of FRITIOF.
C.......................................................................
C... This program generates a few sample FRITIOF events, and then does
C... histogram for negatively charged particle multiplicity distribution
C... in O+Au collision at 200 GeV/nucleon lab energy.
C... The purpose here is to illustrate the typical usage of FRITIOF.
C... This routine, loaded together with FRITIOF_70, ARIADNE_4.02r,
C... PYTHIA_55 and JETSET_73, can be used to test the installation of
C... these programs.
C---------------------------------------------------------------------
 
      SUBROUTINE FRSAMPLE
C                                             -- A SAMPLE MAIN PROGRAM
      PARAMETER (KSZJ=4000,KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      DIMENSION MP(0:300)
      SAVE /FRPARA1/,/FRINTN0/,/LUDAT1/,/LUJETS/,/LUDAT3/
 
C...Open a file to take the write out of the program:
      MSTU(11) = 20
#if defined(CERNLIB_IBM)
      OPEN(MSTU(11),FILE='/test.out',STATUS='unknown')
#endif
#if !defined(CERNLIB_IBM)
      OPEN(MSTU(11),FILE='test.out',STATUS='unknown')
#endif
 
C:::::::Multiplicity distribution for O+AU collision at 200 GeV ::::::::
 
C...Forbid the decays of Lambda and K_S0:
      MDCY(LUCOMP(3122),1) = 0
      MDCY(LUCOMP(310),1) = 0
 
C...Book spaces for the histogram (or use a histogram package):
      DO 50 J=0,300
50    MP(J) = 0
 
C...Test 10 events (of course a lot more events are needed realistically):
      NEVENT=10
      NTRIG = 0
      DO 100 I=1, NEVENT
 
      CALL FREVENT('FIXT','O','AU',200.)
 
C...Output the event using JETSET routine LULIST:
      IF(I.LE.3) CALL LULIST(1)
 
C...Edit the event record, remove partons or decayed particles:
      CALL LUEDIT(1)
 
C...Assume a trigger requiring that the energy in the forward cone
C...(theta < 0.3 degree) must be less than 60% of the total beam energy.
C...Also find out the number of negatively charged particles:
      IQTRIG = 0
      EFWD = 0.
      N_=0
      DO 70 J=1, N
      THETA = PLU(J,14)
C...  (PLU is a JETSET function.  Please refer to the JETSET manual.)
      IF(THETA.LT.0.3) EFWD = EFWD+PLU(J,4)
 
C...Count the negative particles.  Spectator nuclei, which have codes
C...ABS(K(J,2))=10000+N_proton, must be excluded:
      IF(ABS(K(J,2)).LT.10000) THEN
        IF(PLU(J,6).LT.0.) N_ = N_+1
      ENDIF
70    CONTINUE
 
      EBEAM = 200.*IOP(3)
      IF(EFWD.LT.0.6*EBEAM) IQTRIG = 1
 
C...Do histogram:
      IF(IQTRIG.EQ.1) THEN
      NTRIG = NTRIG+1
      MP(N_) = MP(N_)+1
      ENDIF
 
100   CONTINUE
 
C...Output the histogram data:
      WRITE(MSTU(11),500) NEVENT, NTRIG
      DO 200 J=0,300
200   WRITE(MSTU(11),*) J, FLOAT(MP(J))/FLOAT(NTRIG)
500   FORMAT(1X,'Number of events:',I4,2x,'Triggered events:',I4)
 
C...Write out the values of the parameters and some statistics:
      CALL FRVALUE(0)
 
      CLOSE (MSTU(11))
      END
