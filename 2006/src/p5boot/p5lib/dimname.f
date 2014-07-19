CDECK  ID>, DIMNAME.
      SUBROUTINE DIMNAME (LOLDUP,LNEWUP,LEVEL,IFLINK,IST)

C-    Match the dependent banks of the "old" structure LOLDUP to LNEWUP
C-        LEVEL = 0/1  dependent banks are decks/patches
C-       IFLINK = 0/1  do not/do link the matching banks
C-    return IST = 0  exact match
C-                >0  otherwise
C-        IQUEST(1) = number of matching banks
C.    started 3-june-94

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
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
C--------------    End CDE              --------------------------------
      EQUIVALENCE (NLEN,IQUEST(1))

      IOR  (IZV,IZW) =  OR (IZV, IZW)


      NMATCH = 0
      ISTAT  = 0
      LOLDF  = LQ(LOLDUP-4)
      LOLDF  = LQ(LOLDF-1)
      LOLD   = LOLDF

      LNEWF  = LQ(LNEWUP-4)
      LNEWF  = LQ(LNEWF-1)

      IF (LOLDF+LNEWF.EQ.0)        GO TO 37
      IF (LOLDF.NE.0)  THEN
          IF (LNEWF.NE.0)          GO TO 27
        ENDIF
      ISTAT = 1
      GO TO 39

C--       match each as-yet unmatched bank of the OLD structure

   24 CALL DIMSTR (LOLD, LNEWF, LEVEL, IFLINK, IRC)
      NMATCH = NMATCH + NLEN
      ISTAT  = MAX (ISTAT,IRC)

      IF (IRC.LT.2)                GO TO 31

   26 LOLD = LQ(LOLD-1)
      IF (LOLD.EQ.0)               GO TO 31
   27 IF (LQ(LOLD-2).EQ.0)         GO TO 24
      GO TO 26

C--       end of OLD reached

   31 IF (ISTAT.NE.0)              GO TO 39

C--       exact match of the whole structure

   37 IF (IFLINK.EQ.0)             GO TO 39
      IQ(LOLDUP) = IOR (IQ(LOLDUP),KM1+KM2+KM3)
      IQ(LNEWUP) = IOR (IQ(LNEWUP),KM1+KM2+KM3)

   39 IQUEST(1) = NMATCH
      IST  = ISTAT
      RETURN
      END
