*
* $Id: arinit.F,v 1.1.1.1 1996/03/08 16:50:58 mclareni Exp $
*
* $Log: arinit.F,v $
* Revision 1.1.1.1  1996/03/08 16:50:58  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arinit.F,v 1.1.1.1 1996/03/08 16:50:58 mclareni Exp $
C**********************************************************************C
C                                                                      C
C                            A R I A D N E                             C
C                                                                      C
C           A Monte Carlo program for colour dipole radiation          C
C                                                                      C
C                        Version 4 revision 07                     C
C                  Latest date of change: Aug 15 1995                  C
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
C                    E-mail lonnblad@ips102.desy.de                    C
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
 
      SUBROUTINE ARINIT(MODE)
 
C...ARiadne subroutine INITialize
 
C...Initializes Ariadne to run with other (Lund) MC programs
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
      CHARACTER MODE*(*)
 
 
C...Set output files if not already done
      IF (MSTA(7).LT.0) MSTA(7)=MSTU(11)
      IF (MSTA(8).LT.0) MSTA(8)=MSTU(11)
 
C...Set Drell-Yan flagg
      QQ(MAXPAR-2)=.FALSE.
 
C...Write out header
      WRITE(MSTA(7),1000)
      MSTA(2)=1
 
C...If Ariadne mode, do nothing special
      IF (MODE(1:7).EQ.'ARIADNE'.OR.MODE(1:7).EQ.'ariadne') THEN
        MSTA(1)=0
 
C...If JETSET mode, switch off cascade and fragmentation in JETSET
      ELSEIF (MODE(1:6).EQ.'JETSET'.OR.MODE(1:6).EQ.'jetset') THEN
        MSTA(1)=1
        MSTA(5)=MIN(MAX(MSTJ(105),0),1)
        MSTJ(101)=5
        MSTJ(41)=0
        MSTJ(105)=0
        WRITE(MSTA(7),1010)
 
C...If PYTHIA mode, switch off cascades and fragmentation. Check that
C...Ariadne can handle selected processes
      ELSEIF (MODE(1:6).EQ.'PYTHIA'.OR.MODE(1:6).EQ.'pythia') THEN
 
        MSTA(1)=2
        WRITE(MSTA(7),1020)
        MSTA(5)=MIN(MAX(MSTP(111),0),1)
        MSTP(61)=0
        MSTP(71)=0
        MSTP(111)=0
        MSTP(126)=40
 
C...If LEPTO mode, switch off cascades and fragmentation.
      ELSEIF (MODE(1:5).EQ.'LEPTO'.OR.MODE(1:5).EQ.'lepto') THEN
        MSTA(1)=3
        WRITE(MSTA(7),1030)
        IF (MSTA(32).EQ.0) THEN
          LST(8)=9
        ELSE
          LST(8)=0
        ENDIF
        MSTA(5)=MIN(MAX(LST(7),0),1)
        LST(7)=0
        LST(34)=0
 
C...Warn if mode is none of the above
      ELSE
        WRITE(MSTA(7),1040) MODE
        MSTA(1)=0
      ENDIF
 
C...Set quark masses
      IF (MSTA(24).GT.0) THEN
        DO 100 I=1,8
          PQMAS(I)=PMAS(I,1)
 100    CONTINUE
      ENDIF
 
      IF (MSTA(24).GE.2) THEN
        DO 110 I=1,5
          PQMAS(I)=PARF(100+I)
 110    CONTINUE
      ENDIF
 
      IF (MSTA(3).EQ.1) CALL ARTUNE('EMC')
 
 1000 FORMAT(/,12X,
     $     'The Lund Monte Carlo - Ariadne version 4 revision 07',
     $     /,23X,'Latest date of change: Aug 15 1995')
 1010 FORMAT(18X,'Initialization done for running with JETSET')
 1020 FORMAT(18X,'Initialization done for running with PYTHIA')
 1030 FORMAT(18X,'Initialization done for running with LEPTO')
 1040 FORMAT(/,15X,'WARNING: Ariadne cannot be initialized for "',A,
     $     '".',/,21X,'Using default initialization instead.')
 
      RETURN
 
C**** END OF ARINIT ****************************************************
      END
