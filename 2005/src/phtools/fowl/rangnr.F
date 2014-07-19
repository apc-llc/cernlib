*
* $Id: rangnr.F,v 1.1.1.1 1996/03/22 16:42:47 mclareni Exp $
*
* $Log: rangnr.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:47  mclareni
* Phtools
*
*
      SUBROUTINE RANGNR
C--       CALLED FROM GENEV.  ASSEMBLES RANDOM NUMBERS FOR ONE EVENT.
C--       IT INCLUDES THE QUASIRANDOM GENERATOR QQRNDM,
C--       AND USES A PSEUDORANDOM GENERATOR (RNDM) FOR SHUFFLING
#if defined(CERNLIB_CDC)
      COMMON / /  NDIMEN, NRAN, SHUFF(100)
     +/SHUFFL/ RNO(50)   ,NTNM4    ,NTM2     ,NTM1     ,QQCONS(50) ,TIR
      DO 100 I= 1, NTM2
      KULL = RNDM(-1) * 20.
      KULL = KULL + 20*I - 19
      RNO(I) = SHUFF(KULL)
      Z = TIR    * QQCONS(I)
  100 SHUFF(KULL) =  MOD(Z, 1.0)
#endif
#if !defined(CERNLIB_CDC)
      COMMON
     +/SHUFFL/ RNO(50)   ,NTNM4    ,NTM2     ,NTM1     ,QQCONS(50) ,TIR
      CALL NRAN(RNO,NTNM4)
#endif
C--       ORDER THE FIRST NTM2 RANDOM NUMBERS
C--         TWO IS A SPECIAL CASE (FASTER)
      IF(NTM2 - 2) 200,160,110
  110 KM1 = NTM2 - 1
      DO 150 I= 1, KM1
      IQUIT = 0
      NI = NTM2 - I
      DO 140 J= 1, NI
      IF(RNO(J) - RNO(J+1)) 140,140,120
  120 SAV = RNO(J)
      RNO(J) = RNO(J+1)
      RNO(J+1) = SAV
      IQUIT = 1
  140 CONTINUE
      IF(IQUIT) 200,200,150
  150 CONTINUE
      GOTO 200
  160 IF(RNO(1).LE.RNO(2)) GOTO 200
      SAV = RNO(1)
      RNO(1) = RNO(2)
      RNO(2) = SAV
  200 CONTINUE
#if defined(CERNLIB_CDC)
C--       CHOOSE THE REST OF THE RANDOM NUMBERS
      DO 300 I= NTM1, NTNM4
      KULL = RNDM(-1) * 20.
      KULL = KULL + 20*I - 19
      RNO(I) = SHUFF(KULL)
      Z = TIR    * QQCONS(I)
  300 SHUFF(KULL) =  MOD(Z, 1.0)
      TIR = TIR + 1.000
#endif
      RETURN
      END
