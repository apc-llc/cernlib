CDECK  ID>, INDCRP.
      SUBROUTINE INDCRP (IXNAME,MODE)

C-    Register patch name IXNAME for nyindex,
C-        MODE = 0  this is the name of the current patch just starting
C-               1  patch name quoted in IF=
C.    started 17-jan-94

      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
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
      IOR  (IZV,IZW) =  OR (IZV, IZW)


      LPAT = LQFIND (IXNAME,2,LQUSER)

      IF (MODE.EQ.0)               GO TO 21
      IF (LPAT.EQ.0)               GO TO 24
      GO TO 27

C--       start of patch

   21 IF (LPAT.EQ.0)               GO TO 24
      IF (IQ(LPAT+1).GE.0)         GO TO 24
   22 LEXP = LPAT
      IQ(LEXP+1) = NQDKNO
      RETURN

C--       lift the PAT bank

   24 CALL MQLIFT (LPAT, LQUSER(1),1, JBKPAT,3)
      IQ(LPAT+2) = IXNAME
      IF (MODE.EQ.0)               GO TO 22
      IQ(LPAT+1) = -1

C--       mark patch-name used in IF=

   27 IQ(LPAT) = IOR (IQ(LPAT),KM6)
      RETURN
      END
