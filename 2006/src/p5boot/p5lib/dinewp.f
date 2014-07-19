CDECK  ID>, DINEWP.
      SUBROUTINE DINEWP

C-    Create a proxy for each new patch not present on OLD
C.    started 8-mar-95

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------


C----         scan all patches on NEW

      NOP = 0
      LFI = LQUSER(2)
      LPN = LFI - 3
   62 LPN = LQ(LPN-1)
      IF (LPN.EQ.0)                GO TO 87
      LPX = LQ(LPN-2)
      IF (LPX.EQ.0)                GO TO 71

C----         scan all decks of this patch

   63 LDN = LPN - 3
   64 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                GO TO 62
      LDX  = LQ(LDN-2)
      IF (LDX.EQ.0)                GO TO 64

C--       check the deck is of foreign origin
      IF (IQ(LDN+6).GE.4)          GO TO 64

C--       remember this deck for adding to it
      LASTXQ = LDX
      GO TO 64

C------       this patch is new, add a proxy in the OLD structure

   71 NOP = NOP + 1
      LLD = LASTXQ
      LLP = LQ(LLD-3)

      CALL MQLIFT (LPIN,  LLP,-1, JBKPAT,3)
      LQ(LPIN-3) = LQ(LLP-3)
      IQ(LPIN+1) = 1
      IQ(LPIN+4) = IQ(LPN+4)
      IQ(LPIN+5) = IQ(LLP+5)
      IQ(LPIN+9) = -1

      CALL MQLIFT (LDIN, LPIN,-4,  JBKDEC,3)
      LQ(LDIN-3) = LPIN
      IQ(LDIN+5) = IQ(LLD+5)
      IQ(LDIN+9) = -1
      LASTXQ = LDIN

C--       match the inserted patch on OLD with the patch on NEW

      CALL DILINK (LPIN,LPN,1,-1)

C--       shunt the trailing decks of the old to the inserted patch
C--       break any match in the trailing decks

      LLN = LQ(LLD-1)
      IF (LLN.EQ.0)                GO TO 62

      JSLN = IQ(LLN+7)
      JTXN = IQ(LLN+8)

      JSLF = IQ(LLP+7)
      JTXF = IQ(LLP+8)

      NSL1 = JSLN - JSLF
      NTX1 = JTXN - JTXF

      NSL2 = IQ(LLP+2) - NSL1
      NTX2 = IQ(LLP+3) - NTX1

      IQ(LLP+2) = NSL1
      IQ(LLP+3) = NTX1

      IQ(LPIN+1) = 0
      IF (IQ(LLP+1).NE.0)  IQ(LPIN+1)= IQ(LLP+1) + NSL1
      IQ(LPIN+2) = NSL2
      IQ(LPIN+3) = NTX2
      IQ(LPIN+4) = IQ(LLP+4)
      IQ(LPIN+7) = JSLF + NSL1
      IQ(LPIN+8) = JTXF + NTX1

      IQ(LDIN+1) = IQ(LPIN+1)
      IQ(LDIN+7) = IQ(LPIN+7)
      IQ(LDIN+8) = IQ(LPIN+8)

      CALL QSHLIN (LLD-1, LDIN-1)
      CALL DINOUN (LLP)

      LD = LDIN
   74 LD = LQ(LD-1)
      IF (LD.NE.0)  THEN
          LQ(LD-3) = LPIN
          LM = LQ(LD-2)
          IF (LM.EQ.0)       GO TO 74
          LQ(LD-2) = 0
          LQ(LM-2) = 0
          IQ(LM+5) = 0
          IQ(LM+9) = 0
          GO TO 74
        ENDIF

C--       match the decks by name

      CALL DIMNAME (LPIN,LPN, 0, 1, IRC)

C--       match the decks by content

      CALL DISAMD (LPIN,LPN)
      GO TO 63

   87 CONTINUE
      RETURN
      END
