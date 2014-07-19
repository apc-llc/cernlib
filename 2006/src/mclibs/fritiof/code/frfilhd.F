*
* $Id: frfilhd.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frfilhd.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C************************ END FRCHEXG ***********************************
 
 
C********************************* FRFILHD ******************************
 
      SUBROUTINE FRFILHD(I,IQ,KFEL)
 
C....TO INCORPORATE THE HARD PARTON MOMENTA INTO THE NUCLEON SYSTEM AND
C....TO EVELUATE THE SYSTEM MASS PPSY(,,5).
C......IQ = 0, PPSY is updated but hard partons not stored (PPH not updated);
C......IQ = 1, no effect to PPSY but PPH, PHP are updated
C......IQ <0: hard partons stripped off from the record.
C......Output flag for IQ=0: (kfel is dummy for ABS(IQ)=1)
C......Kfel=0, no problem;
C......    =4, system mass smaller than minimum, FRFILHD aborts;
 
      PARAMETER (KSZ1=20,KSZ2=300)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRINTN0/PLI0(2,4),AOP(KSZ1),IOP(KSZ1),NFR(KSZ1)
      COMMON/FRINTN3/IDN(2,KSZ2),FMN(2,KSZ2),NUC(2,3000)
      COMMON/FRJETS/NJ,KJ(100,5),PJ(100,5)
      COMMON/FRINTN2/NHP(2),IHQP(2,KSZ2),KHP(2,KSZ2,100,5),
     >   PHP(2,KSZ2,100,5)
      COMMON/FRINTN1/PPS(2,KSZ2,5),PPH(2,KSZ2,5),PPSY(2,KSZ2,5),PPA(2,5)
 
      DIMENSION PPHN(2,4),IHQPM(2)
      SAVE PPHN
      SAVE /FRPARA1/,/FRINTN0/,/FRINTN3/,/FRJETS/,/FRINTN2/,/FRINTN1/
 
      kfel=0
 
      IF(IQ.EQ.0) THEN
 
      DO 11 L=1,2
      DO 11 J=1,4
11    PPHN(L,J) = 0.
 
      IF(NJ.GE.1) THEN
      DO 21 LO=1, NJ
      L = ABS(KJ(LO,3))
      PPHN(L,1) = PPHN(L,1)+ PJ(LO,1)
      PPHN(L,2) = PPHN(L,2)+ PJ(LO,2)
      PPHN(L,3) = PPHN(L,3)+ PJ(LO,4)-PJ(LO,3)
21    PPHN(L,4) = PPHN(L,4)+ PJ(LO,4)+PJ(LO,3)
      ENDIF
 
      DO 30 L=1, 2
      DO 30 LO=1, 4
30    PPSY(L,NUC(L,I),LO) = PPS(L,NUC(L,I),LO)+
     >               PPH(L,NUC(L,I),LO)+ PPHN(L,LO)
 
      DO 570 L=1,2
      SMSY2 = PPSY(L,NUC(L,I),4)*PPSY(L,NUC(L,I),3)-
     >    PPSY(L,NUC(L,I),1)**2-PPSY(L,NUC(L,I),2)**2
      IF(SMSY2.LT.AOP(8+L)**2) THEN
      KFEL=4
      RETURN
      ENDIF
570   PPSY(L,NUC(L,I),5) = SQRT(SMSY2 )
 
      ELSEIF(IQ.EQ.1.and.NJ.GT.0) THEN
C.....................................STORE THE HARD PARTONS TO FRINTN2
 
       IHQPM(1) = IHQP(1,NUC(1,I))
       IHQPM(2) = IHQP(2,NUC(2,I))
       DO 512 LO = 1, NJ
       ISIDE = ABS(KJ(LO,3))
       INUC = NUC(ISIDE,I)
       IHQP(ISIDE,INUC) = IHQP(ISIDE,INUC) +1
       NHP(ISIDE) = NHP(ISIDE) + 1
       DO 510 L=1,4
       PHP(ISIDE,INUC,IHQP(ISIDE,INUC),L)= PJ(LO,L)
510    KHP(ISIDE,INUC,IHQP(ISIDE,INUC),L)= KJ(LO,L)
       IF(kfr(9).NE.0) KHP(ISIDE,INUC,IHQP(ISIDE,INUC),4)=IHQPM(ISIDE)
512    CONTINUE
 
       DO 90 L = 1, 2
       DO 90 L2 = 1,4
 90    PPH(L,NUC(L,I),L2) = PPH(L,NUC(L,I),L2)+ PPHN(L,L2)
 
       ELSEIF(IQ.LE.-1.and.NJ.GT.0) THEN
C....................................STRIP OFF THE HARD PARTONS
 
        DO 520 LO = 1, NJ
        ISIDE = ABS(KJ(LO,3))
        INUC = NUC(ISIDE,I)
         DO 517 L=1,4
         PHP(ISIDE,INUC,IHQP(ISIDE,INUC),L)= 0.0
517      KHP(ISIDE,INUC,IHQP(ISIDE,INUC),L)= 0.0
        NHP(ISIDE) = NHP(ISIDE) - 1
520     IHQP(ISIDE,INUC) = IHQP(ISIDE,INUC) -1
 
        DO 95 L = 1, 2
        DO 95 L2 = 1,4
 95     PPH(L,NUC(L,I),L2) = PPH(L,NUC(L,I),L2)- PPHN(L,L2)
 
      NJ = 0
 
      ENDIF
 
      RETURN
      END
