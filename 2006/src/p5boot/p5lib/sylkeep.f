CDECK  ID>, SYLKEEP.
      SUBROUTINE SYLKEEP (LSEQ)

C-    Find the KEEP bank for the sequence decribed by
C-    the current +KEEP control line;
C-    create target PAT/DECK banks if local sequence.

C-    return   LSEQ       L-adr of the sequence found, or LSEQ=0
C-             IQUEST(1)  K-adr of the seq found, if found
C-                        else: K-adr for attachment at end
C-             IQUEST(2)  zero:  sequence is global
C-                         -ve:  sequence is local
C-             IQUEST(11) zero if global sequence
C.    started 29-aug-92

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      CHARACTER      CCKORG*256, CCKARD*256, CCCOMF*256
      COMMON /CCPARA/NCHCCD,NCHCCT, JCCTYP,JCCLEV,JCCSL,MCCPAR(240)
     +,              NCCPAR,MXCCIF,JCCIFV,JCCBAD,JCCWAR,ICCSUB,JCCWK(4)
     +,              JCCPP,JCCPD,JCCPZ,JCCPT,JCCPIF,JCCPC,JCCPN
     +,              NCCPP,NCCPD,NCCPZ,NCCPT,NCCPIF,NCCPC,NCCPN
     +,              JCCEND, NCHCCC,IXCCC,  CCKORG, CCKARD, CCCOMF
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

      IAND (IZV,IZW) = AND (IZV, IZW)


      IXS = MCCPAR(JCCPZ+1)
      CALL SYLSEQ (L, IXS, 0,0)
      IQUEST(11) = 0
      IF (L.NE.0)                  GO TO 22
      IF (NCCPP.EQ.0)              GO TO 29
      GO TO 23

C--       global sequence exist, check expansion restricted

   22 IF (IAND(IQ(L),KM12+KM13).NE.0)   GO TO 29
      IF (NCCPP.EQ.0)                   GO TO 29
      IF    (IAND(IQ(L),KM14).NE.0) THEN
         IF (IAND(IQ(L),KM15).NE.0)     GO TO 29
       ENDIF

   23 IXP = MCCPAR(JCCPP+1)
      IXD = MCCPAR(JCCPD+2)
      KD  = 0

      CALL CREAPD (IXP, IXD, 0)
      KP  = LCRP - 3
      IF (LCRD.NE.0)  KD = LCRD - 3

   24 CALL SYLSEQ (L, IXS, KP,KD)
      IQUEST(11) = 7
   29 LSEQ = L
      RETURN
      END
