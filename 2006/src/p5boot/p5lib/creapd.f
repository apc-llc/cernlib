CDECK  ID>, CREAPD.
      SUBROUTINE CREAPD (IXPAX,IXDEX,MODELK)

C-    Locate or create for name index (IXPAT,IXDEC)

C-              banks 'PAT' and 'DECK'
C-              at     LCRP and  LCRD

C-        if  IXPAT= 0    use PAT bank existing at LCRP
C-        if  IXDEC= -ve  no DECK please, return LCRD=0

C-    Linkage control :
C-             MODELK  -ve  create without search
C-                       0  normal search and creation
C-                     +ve  LEXP is to point to the resultant PAT bank
C-                          shunt the bank currently at LEXP to LQPAST

C-    Returns  LCRP and LCRD  of PAT / DECK banks found or created

C-             NVUTY(1)  K-adr of PAT bank
C-             NVUTY(2)  K-adr of DECK bank

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
      PARAMETER (KQGARB=1,KQARRV=3,KQKEEP=4,KQPREP=5,KQMAIN=6,KQPAST=8)
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
C--------------    End CDE              --------------------------------
      DIMENSION    IXPAX(9), IXDEX(9)

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      IXPAT = IXPAX(1)
      IXDEC = IXDEX(1)
      MODE  = MODELK

C------            Find or create PAT-bank

      IF (IXPAT.EQ.0)              GO TO 41
      LORG = 0
      KP   = LEXP-1
      IF (MODE.LT.0)               GO TO 32
      IF (MODE.GT.0)  THEN
          CALL QSHUNT (KQMAIN,KQPAST)
          KP = KQMAIN
        ENDIF

C--                Find 'Future PAT'-bank pre-existing

      LQ(LLPAST-1) = 0
      LCRP = KQFIND (IXPAT,2,KQMAIN,NVUTY(1))
      IF (LCRP.EQ.0)               GO TO 31
      IF (MODE.EQ.0)               GO TO 41
      CALL QSHUNT (NVUTY(1),KP)
      GO TO 38

C--                Find pre-existing  'Past PAT'-bank

   31 LORG = LQFIND (IXPAT,2,LQPAST)

C--                Create new PAT-bank

   32 CALL MQLIFT (LCRP,KP,0,JBKPAT,3)
      IQ(LCRP)   = IOR (IQ(LCRP), MU_GLOB)
      IQ(LCRP+2) = IXPAT
      IF (LORG.EQ.0)               GO TO 38
      CALL MXJOIN (IQ(LORG),IQ(LCRP))
      IQ(LCRP+1) = IQ(LORG+1)
      LQ(LCRP-2) = LQ(LORG-2)
      LQ(LCRP-3) = LQ(LORG-3)
      LQ(LCRP-4) = LQ(LORG-4)
      LQ(LORG-2) = 0
      LQ(LORG-3) = 0
      CALL SBIT0 (IQ(LCRP+1),1)

C--       set reverse pointers in DECK banks

      L = LCRP - 1
   34 L = LQ(L-1)
      IF (L.EQ.0)                  GO TO 38
      LQ(L-4) = LCRP
      GO TO 34

   38 NVUTY(1) = KP

C----              Locate DEC-bank, unless  IXDEC=-1

   41 IF (IXDEC.LT.0)              GO TO 57
      LCRD = KQFIND (IXDEC,1,LCRP-2,NVUTY(2))
      IF (LCRD.NE.0)               RETURN

      CALL MQLIFT (LCRD,NVUTY(2),0,JBKDEC,1)
      CALL SBYT (IQ(LCRP),IQ(LCRD),1,18)
      IQ(LCRD+1) = IXDEC
      LQ(LCRD-4) = LCRP
      RETURN

   57 LCRD = 0
      RETURN
      END
