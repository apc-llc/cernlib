*
* $Id: fringeb.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: fringeb.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C******************************** END FRHILDN ****************************
 
C******************************** FRINGEB  *******************************
 
      SUBROUTINE FRINGEB
 
C........................This routine administrates one complete event
 
      PARAMETER (KSZJ=4000,KSZ1=20,KSZ2=300)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRINTN4/KFEND(2,KSZ2,2)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRGEOMC/NFLG,NUMROP,NUMROT,NUMREP
      COMMON/FRCONT2/ICT(10),ICTT(10)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
 
      SAVE /FRINTN0/,/FRINTN1/,/FRINTN3/,/FRINTN4/,/FRPARA1/,/FRGEOMC/,
     >     /FRCONT2/,/LUJETS/,/LUDAT1/
 
      NFR(1) = NFR(1) + 1
  2   IOP(16)=0
 
C.....Randomly order protons and neutrons:
      CALL FRHILDN
 
C.....Create the nuclei and calculate number of collisions
      CALL FRANGAN
 
C.....Fix the end flavors for wounded strings
      DO 5 L=1,2
      DO 5 II=1, IOP(9+(L-1))
   5  CALL FRBELEO(KFEND(L,II,1),KFEND(L,II,2),IDN(L,II))
 
C.....Generate the masses and momenta after the collisions
 10   CALL FRRINGO
 
      MSTU24=0
      NMEM=0
C.. Fill the strings and emit gluons:
      N=0
      NSTR = 0
      IOP(15) = 0
      DO 100 L=1, 2
      DO 100 J=1,IOP(8+L)
      CALL FRTORST(L,J)
      NSTR = NSTR+1
      NQG = N-NMEM
               IF( (KFR(1).EQ.1.AND.KFR(13).GE.1).AND.
     >             (NSTR.GT.100.OR.NQG.GT.(KSZJ-N)/10) ) THEN
      CALL LUEXEC
      IF(MSTU(24).EQ.4) MSTU24=1
          IF(KFR(13).LE.3) THEN
        CALL LUEDIT(KFR(13))
          ELSEIF(KFR(13).GE.4) THEN
        CALL FREDITD()
          ENDIF
      NMEM=N
      NSTR=0
               ENDIF
100   CONTINUE
 
C....TO ADD ONTO LUJETS THE COLOUR NEUTRAL PARTICLES THAT MAY
C....HAVE BEEN PRODUCED FROM PARTON-PARTON PROCESSES:
      CALL FRFILHW
 
      IF(N.GE.KSZJ-2) CALL FRMGOUT(0,1,
     > 'LUJETS array size KSZJ must be expanded',float(N),float(KSZJ),
     >  0.,0.,0.)
 
      IF(KFR(1).EQ.1.AND.N.GT.NMEM) THEN
      CALL  LUEXEC
      IF(MSTU(24).EQ.4) MSTU24=1
        IF(KFR(13).GE.1.AND.KFR(13).LE.3) THEN
      CALL LUEDIT(KFR(13))
        ELSEIF(KFR(13).GE.4) THEN
      CALL FREDITD()
        ENDIF
      ENDIF
 
C... Regenerate event in case of 'infinite-loop error' in Jetset:
      IF(MSTU24.GT.0) GOTO 2
 
C....Record the number of N-N collisions:
      IOP(2) = IOP(2)-ICT(3)-ICT(8)-ICT(10)
      NFR(3) = NFR(3) + IOP(2)
 
       NFR(4) = NFR(4) + IOP(13)
       IF(IOP(13).GE.1) NFR(5) = NFR(5)+1
 
C-----RECORDING OF IMPACT PARAMETER AND COUNTING OF SPECTATOR PROTONS
 
      IOP(11)=IOP(4)
      IOP(12)=IOP(6)
      DO 200 L=1, 2
      DO 200 J=1,IOP(9+L-1)
      IF(IDN(L,J).EQ.2212) IOP(10+L)=IOP(10+L)-1
  200 CONTINUE
 
      IF(IDN(1,1).NE.2212.AND.IDN(1,1).NE.2112) IOP(11)=0
 
C.....Add the nuclei spectators onto the event record:
      DO 300 L=1,2
      IF(IOP(3+2*(L-1))-IOP(8+L).GE.1) THEN
      N = N+1
      P(N,1)=PPA(L,1)
      P(N,2)=PPA(L,2)
      P(N,3)=0.5*(PPA(L,4)-PPA(L,3))
      P(N,4)=0.5*(PPA(L,4)+PPA(L,3))
      P(N,5)=PPA(L,5)
      K(N,1) = 4
      K(N,2) = (10000+IOP(10+L))*(-1)**(L-1)
      K(N,3) = 0
      K(N,4) = 0
      K(N,5) = 0
      ENDIF
300   CONTINUE
 
C....Check Energy, momentum and charge conservation:
      IF(KFR(14).EQ.1) CALL FRCHKEP(1)
 
C....Dump out the event for inspection when error occurs in FRMGOUT:
      IF(IOP(16).GE.1) CALL LULIST(2)
 
      RETURN
      END
