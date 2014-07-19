CDECK  ID>, MXOPER.
      SUBROUTINE MXOPER (MERACT)

C-    Unpack the USE bits in MU_DECK ready for use
C-    MERACT = EXE bits from actions into the deck
C-             if zero: initial unpack, if not:  merge MERACT
C.    started  3-mar-92

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
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
      IAND (IZV,IZW) = AND (IZV, IZW)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
      ISHFTR (IZW,NZB) = ISHFT (IZW, -NZB)

C--            MERACT = O : initial split of MU_DECK

      IF (MERACT.NE.0)             GO TO 21

      MU_FORG = IAND        (MU_DECK,     KM6-1)
      MU_INH  = IAND (ISHFTR(MU_DECK,5),  KM6-1)
      MX_TRAN = IAND (ISHFTR(MU_DECK,10), KM5-1)
      MX_FORG = IAND (MU_FORG, KM5-1)

      MXOR  =  IOR (MX_FORC,MU_DECK)
      MXAND = IAND (MX_FORC,MU_DECK)

      MX_SINH = IAND (ISHFTR(MXAND,5), KM5-1)
      MX_SELF =  IOR (ISHFTR(MXOR,14), MX_FORG)
      GO TO 22

C--            MERACT not zero, join it

   21 MX_SELF =  IOR (MX_SELF, MERACT)
   22 MX_SELF = IAND (MX_SELF, MX_SINH)

      CALL UPKBYT (MX_SELF,1,NVEXDK,4,0)
      IF (NQERR.NE.0)  NVEXDK(1)= 0
      NVEXDK(5) = NVEXDK(1) + NVEXDK(2)
      NVEXDK(6) = IAND (NVEXDK(2), MOPTIO(6))
      IF (INCRAD.LT.2)             RETURN

C--       ready for P=CRA*, D=blank or CRA*
      NVEXDK(1) = 0
      NVEXDK(2) = 1
      NVEXDK(5) = 1
      NVEXDK(6) = 1
      RETURN
      END
