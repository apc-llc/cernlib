CDECK  ID>, EXLIST.
      SUBROUTINE EXLIST

C-    Run nylist
C.    started 12-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      PARAMETER (KMA=1,KMB=2,KMC=4,KMD=8,KME=16,KMF=32,KMG=64,KMH=128,
     +  KMI=256, KMJ=512, KMK=KM11, KML=KM12, KMM=KM13, KMN=KM14,
     +  KMO=KM15, KMP=KM16, KMQ=KM17, KMR=KM18, KMS=KM19)
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
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
      PARAMETER   (MASK= KMH + KME + KMS)


      CALL INIAUX (LASTWK)

      CALL KROPT (IXLUN(2),MASK)
      CALL PGSIZE (0, IXLUN(2))
      IF (MOPTIO(8).NE.0)          GO TO 81

      IF (MOPTIO(19).NE.0)  THEN
          MOPTIO(19) = 0
        ELSE
          MOPTIO(3) = 1
          MOPTIO(5) = 0
        ENDIF

      NQCMAX  = 104
      NQCPGH  = 100
      NQWYLDO = 7

      CALL INIPAM (11, IXLUN(1), 1,0)
      NQJOIN = -1

C------        Load the title deck to memory      ------------

   21 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

C----          Handle the PAM-file title line

      JDKTYP = 3
      CALL KRTITL
      JSLTTL = IQ(LARRV+10)

      IF (NQDKNO.GE.2)  THEN
          NQDKNO = ((NQDKNO-1)/100 + 1) *100
        ELSE
          NQDKNO = 0
        ENDIF

      CALL LN_GET (JSLTTL, SLLINE, 60)
      NTXT = NDSLAT

      IF (IQPRNT.NE.IQTYPE)  WRITE (IQTYPE,9024) SLLINE(1:NTXT)
 9024 FORMAT (' Read Pam file: ',A/)

C------            Loop over all decks until next title

      CALL LIDECK
      IF (JDKTYP.EQ.3)             GO TO 21
      GO TO 89

C----         Help

   81 WRITE (IQPRNT,9081)

   89 CALL PGSIZE (1, 0)
      RETURN

 9081 FORMAT (' nylist  pam.car  options  print'/
     F/' reads the "pam" file printing on "print" a numbered'
     F/' listing of the whole file. Line numbers both local to'
     F/' each deck and global in the file are shown. Page ejects'
     F/' can be controled with the option letters S and E, and'
     F/' also with T=JOIN given on any deck header line.'/
     F/' "options":  H  help - print this help information only'
     F/'             E  eject - honor the pseudo sequence calls'
     F/'                              +SEQ, QEJECT, N=n.'
     F/'             S  start - start each deck on a new page,'
     F/'                        unless T=JOIN present'
     F/'       0 ... 4  - select page size by single digit option'
     F/'            "n" - set page size to be n > 19')
      END
