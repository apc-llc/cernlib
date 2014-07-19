CDECK  ID>, EXTIDY.
      SUBROUTINE EXTIDY

C-    Run nytidy
C.    started 26-may-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144,
     +  KM20=524288,  KM21=1048576,  KM22=2097152,  KM23=4194304,
     +  KM24=8388608, KM25=16777216, KM26=33554432, KM27=67108864,
     +  KM28=134217728, KM29=268435456, KM30=536870912, KM31=1073741824)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256,  KMJ=512,  KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19, KMT=KM20,
     +  KMU=KM21, KMV=KM22, KMW=KM23, KMX=KM24, KMY=KM25, KMZ=KM26)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      COMMON /DEPCOM/JD_NEW, JD_DTN, JD_DTP,  JD_DTD, JD_DIV
     +,              JD_SML, JD_SMP, JD_SMX,  JDP_DT, JDP_DV
     +,              JD_SPL, JD_MOD, JD_LUN,  JD_LOG, JD_NLI, JD_MULT
     +,              IX_EXT, IX_RH,  IX_SUB
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
     +,       IXBLADK, IXSQSP(7), NTRBLA,NMODIF, JSLTTL,   LASTTI
C--------------    End CDE              --------------------------------
      INTEGER      RENAMEF
      PARAMETER   (MASK= KMB + KMC + KMH + KMV)


      CALL INIAUX (LASTTI)

      CALL KROPT (IXLUN(3),MASK)
      IF (MOPTIO(8).NE.0)          GO TO 81
      MOPTIO(2) = 0

      CALL INIPAM (11, IXLUN(1), 1,0)

      IXBLADK   = NA_LONG ('BLANKDEK')
      IXSQSP(1) = NA_LONG ('DATEQQ')
      IXSQSP(2) = NA_LONG ('TIMEQQ')
      IXSQSP(3) = NA_LONG ('VERSQQ')
      IXSQSP(4) = NA_LONG ('VIDQQ')

      IFLOVW = 0
      IF (IXLUN(2).EQ.0)           GO TO 24

C--           check "new" is a pure directory

      CALL FLSPLIT (IXLUN(2),IXDIR,IXFN,IXEXT)
      IF (IXFN.NE.0)               GO TO 27
      CALL FLSPLIT (IXLUN(1),IXD,IXFN,IXEXT)
      IF (IXD.EQ.IXDIR)  CALL P_KILL ('clashing file names')
      GO TO 26

C--           derive the name of "new" if not given

   24 IFLOVW = 7
      CALL FLSPLIT (IXLUN(1),IXDIR,IXFN,IXEXT)
      IXEXT = NA_LONG ('.tdy')
   26 CALL FLJOIN (IXDIR,IXFN,IXEXT,IXLUN(2))

C--           open the output file

   27 JD_LUN = 21
      CALL FLINK  (JD_LUN,6,IXLUN(2),0)
      JD_LUN = LUNFD
      WRITE (IQTYPE,9029) CHLIFI(1:NLIFI)
 9029 FORMAT (' Writing file: "',A,'"'/)

C------        Load the title deck to memory      ------------

   41 IF (JDKNEX.EQ.0)  THEN
          CALL ARRIVE (2)
          CALL TIDISTB
          NTRBLA = NTRBLA + IQUEST(1)
        ENDIF

C----          Handle the PAM-file title line

      JDKTYP = 3
      CALL KRTITL
      JSLTTL = IQ(LARRV+10)
      CALL USETT (JSLTTL)

      CALL LN_GET (JSLTTL, SLLINE, 72)
      NTXT = NDSLAT

      CALL DPBLAN (0)
                             WRITE (IQPRNT,9044) SLLINE(1:NTXT)
      IF (IQPRNT.NE.IQTYPE)  WRITE (IQTYPE,9044) SLLINE(1:NTXT)
 9044 FORMAT (' Read Pam file: ',A/)

C------            Loop over all decks until next title

      CALL TIDECK
      IF (JDKTYP.EQ.3)             GO TO 41

C--       close output
      CALL CICLOS (JD_LUN)
C--       close input
      CALL INIPAM (0,0, -1,0)

      CALL DPBLAN (0)
                            WRITE (IQPRNT,9071) NMODIF,NTRBLA
      IF (IQPRNT.NE.IQTYPE) WRITE (IQTYPE,9071) NMODIF,NTRBLA
 9071 FORMAT (I8,' changes done,',I7,' lines with trailing blanks.'/)

      IF (IFLOVW.EQ.0)             RETURN

C--       delete the new file if overwriting and unchanged

      IF (NMODIF+NTRBLA.EQ.0)  THEN
          CALL FLNAME (IXLUN(2),N)
          CALL UNLINKF (CHLIFI(1:NLIFI))
          RETURN
        ENDIF

C--           rename "new" to "pam"

      CALL FLNAME (IXLUN(1), N)
      NIN = NLIFI
      SLLINE(1:NIN) = CHLIFI(1:NIN)

      CALL FLNAME (IXLUN(2),N)

      WRITE (IQTYPE,9077) CHLIFI(1:NLIFI),SLLINE(1:NIN)
 9077 FORMAT (' Renaming file: ',A
     F/       '            to: ',A/)

      ISTAT = RENAMEF (CHLIFI(1:NLIFI), SLLINE(1:NIN))

      IF (ISTAT.EQ.0)              RETURN
      CALL P_KILLM ('Rename new to old failed')
      RETURN

C----         Help

   81 WRITE (IQPRNT,9081)
      RETURN

 9081 FORMAT (' nytidy  pam.car  new.car  options  print'/
     F/' copies "pam" to "new" with some tidying-up:'
     F/'  - remove trailing blanks'
     F/'  - remove lead/trailing comment lines in decks of type FORT'
     F/'  - translate non-standard sequence calls'/
     F/' options:  C  the input is a CMZ output file'
     F/'           H  help - print this help information only'
     F/'           V  verbose - print each modification'/
     F/' If "new" is not given the result will overwrite "pam".'
     F/' If "new" is given as a directory (with / at the end) it will'
     F/'          inherit file-name and extension from "pam".')
      END
