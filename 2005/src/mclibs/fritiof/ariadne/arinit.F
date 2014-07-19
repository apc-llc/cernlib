*
* $Id: arinit.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: arinit.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arinit.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
C**********************************************************************C
C                                                                      C
C                            A R I A D N E                             C
C                                                                      C
C           A Monte Carlo program for colour dipole radiation          C
C                                                                      C
C                        Version 4 revision 02                         C
C                  Latest date of change: 30 Apr 1992                  C
C                                                                      C
C                              Author :                                C
C                                                                      C
C                           Leif Lonnblad                              C
C                                                                      C
C                Deutsches Elektronen Synchrotron - DESY               C
C               Notkestrasse 85, 2000 Hamburg 50, Germany              C
C                                                                      C
C                       tel  int+49-4089982048                         C
C                       fax  int+49-4089982777                         C
C                                                                      C
C                   E-mail lonnblad@apollo3.desy.de                    C
C                                                                      C
C                   Copyright (C) 1992 Leif Lonnblad                   C
C                                                                      C
C                Please report any errors to the author                C
C                                                                      C
C**********************************************************************C
 
C**********************************************************************C
C     This program must be loaded together with JETSET 73              C
C     The model is described in Nucl. Phys. B306 (1988) 746,           C
C     Z. Phys. C43 (1989) 625, and Nucl. Phys. B339 (1990) 393.        C
C**********************************************************************C
 
C***********************************************************************
CCPH:   Double precision (B) used.
CCPH:.. This version of ARIADNE is to be used with FRITIOF 7.1
CCPH:.. All Fritiof-associated changes can be searched under "CCPH"
CCPH:   The role of LUJETS common block is replaced by
CCPH:      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
CCPH:.. Since Fritiof requires the strings to be treated one at a time,
CCPH:   the original approach of having entire LUJETS to be treated
CCPH:   by Ariadne is not possible to use.  Therefore this approach
CCPH:   of replacing LUJETS by ARJETX is adopted.  After emission is done,
CCPH:   the partons can be copied back onto LUJETS in Fritiof.
CCPH:
C***********************************************************************
 
      SUBROUTINE ARINIT(MODE)
 
C...ARiadne subroutine INITialize
 
C...Initializes Ariadne to run with other (Lund) MC programs
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
 
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,XQ2,U
      SAVE /LEPTOU/
 
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
      COMMON /PYSUBS/ MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      SAVE /PYSUBS/
      CHARACTER MODE*(*)
 
 
C...Set output files if not already done
      IF(MSTA(7).LT.0) MSTA(7)=MSTU(11)
      IF(MSTA(8).LT.0) MSTA(8)=MSTU(11)
 
C...Write out header
      WRITE(MSTA(7),1000)
      MSTA(2)=1
 
C...If Ariadne mode, do nothing special
      IF(MODE(1:7).EQ.'ARIADNE') THEN
        MSTA(1)=0
 
C...If JETSET mode, switch off cascade and fragmentation in JETSET
      ELSEIF(MODE(1:6).EQ.'JETSET') THEN
        MSTA(1)=1
        MSTA(5)=MIN(MAX(MSTJ(105),0),1)
        MSTJ(101)=5
        MSTJ(41)=0
        MSTJ(105)=0
        WRITE(MSTA(7),1010)
 
C...If PYTHIA mode, switch off cascades and fragmentation. Check that
C...Ariadne can handle selected processes
      ELSEIF(MODE(1:6).EQ.'PYTHIA') THEN
 
        MSTA(1)=2
        WRITE(MSTA(7),1020)
        MSTA(5)=MIN(MAX(MSTP(111),0),1)
        MSTP(61)=0
        MSTP(71)=0
        MSTP(111)=0
 
C...If LEPTO mode, switch off cascades and fragmentation.
      ELSEIF(MODE(1:5).EQ.'LEPTO') THEN
        MSTA(1)=3
        WRITE(MSTA(7),1030)
        LST(8)=9
        MSTA(5)=MIN(MAX(LST(7),0),1)
        LST(7)=0
      ENDIF
 
C...Set quark masses
      IF(MSTA(24).GT.0) THEN
        DO 100 I=1,8
          PQMAS(I)=PMAS(I,1)
 100    CONTINUE
      ENDIF
 
      IF(MSTA(24).GE.2) THEN
        DO 110 I=1,5
          PQMAS(I)=PARF(100+I)
 110    CONTINUE
      ENDIF
 
      IF(MSTA(3).EQ.1) CALL ARTUNE('DELPHI')
 
 1000 FORMAT(/,14X,
     $     'The Lund Monte Carlo - Ariadne version 4 revision 02',/,
     $     23X,'Latest date of change: 30 Apr 1992')
 1010 FORMAT(18X,'Initialization done for running with JETSET')
 1020 FORMAT(18X,'Initialization done for running with PYTHIA')
 1030 FORMAT(18X,'Initialization done for running with LEPTO')
 
 
      RETURN
 
C**** END OF ARINIT ****************************************************
      END
