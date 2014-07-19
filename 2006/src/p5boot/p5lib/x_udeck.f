CDECK  ID>, X_UDECK.
      SUBROUTINE X_UDECK (LPAT)

C-    Merge new USE/EXE-status of patch into all dependent decks
C!    if LPAT = zero: blank deck and D=CRA* of P=CRA* change
C!    their own status

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
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
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
C--------------    End CDE              --------------------------------
      PARAMETER   (IXCRA=2)

      IAND (IZV,IZW) = AND (IZV, IZW)

      LP = LPAT
      IF (LP.EQ.0)                 GO TO 32

      LD  = LQ(LP-2)
      IF (LD.EQ.0)                 RETURN
      MXP = IQ(LP)

   21 CALL MXJOIN (MXP,IQ(LD))
      LD  = LQ(LD-1)
      IF (LD.NE.0)                 GO TO 21
      IF (INCRAD.LT.2)             RETURN
      IF (LP.NE.LEXP)              RETURN

C--       Currently processing P=CRA*, D=blank or CRA*, whose mode
C-        may have changed: update the variables in MUSEBC
C-        note: name index  =0 for blank, =2 for CRA*

   32 MU_PAT = IAND (IQ(LEXP), KM19-1)
      IF (LEXD.EQ.0)  LEXD= KQFIND (IXCRA,1,LEXP-2,KF)
      IF (LEXD.NE.0)  THEN
          MU_DECK = IAND (IQ(LEXD), KM19-1)
        ELSE
          MU_DECK = MU_PAT
        ENDIF

      CALL MXOPER (0)
      END
