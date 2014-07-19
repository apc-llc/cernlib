CDECK  ID>, M_SPLIT.
      FUNCTION M_SPLIT (LDO,JSLN)

C-    Split PREP bank in division 2 into two:

C-    if JSLN not zero: scan the linear structure of PREP banks
C-    starting at LDO to find the bank which supports the line
C-    at slot JSLN; if this is not the first line split this
C-    bank into two to make JSLN the first line in the new bank;
C-    return the adr of the bank with JSLN, or zero if not found.

C-    if JSLN is zero: split the bank at LDO leaving zero lines
C-    in the old bank, transfer the properties of the old to the
C-    new bank; return the adr of the new bank.
C#    started 11-nov-91

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------


      L = LDO
      IF (JSLN.EQ.0)               GO TO 41
      GO TO 22

   21 L = LQ(L-1)
      IF (L.EQ.0)                  GO TO 49
   22 JSLA = IQ(L+1)
      JSLE = IQ(L+2) + JSLA
      IF (JSLN.GE.JSLE)            GO TO 21
      IF (JSLN.EQ.JSLA)            GO TO 49
      IF (JSLN.LT.JSLA)  CALL P_CRASH ('trouble in M_SPLIT')
      NSLA = JSLN - JSLA
      NSLN = JSLE - JSLN

      CALL MQLIFT (LX,L,-1,JBKPRE,2)
      IQ(L+2) = NSLA
      L       = LX
      IQ(L+1) = JSLN
      IQ(L+2) = NSLN
      GO TO 49

C--       split on first line with transfer of properties
C-        (to insert a zero-line PREP reference to ACT for +REPL etc)

   41 CALL MQLIFT (LX,L,-1,JBKPRE,2)

      IQ(LX)   = IQ(L)
      IQ(LX+1) = IQ(L+1)
      IQ(LX+2) = IQ(L+2)
      IQ(L+2)  = 0
      L = LX

   49 M_SPLIT = L
      RETURN
      END
