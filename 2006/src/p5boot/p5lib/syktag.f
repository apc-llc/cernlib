CDECK  ID>, SYKTAG.
      SUBROUTINE SYKTAG (LEVEL,IXKEEP)

C-    Construct the next tail for sequence IXKEEP at level LEVEL
C-    if LEVEL < 0:  simply print the pending tag
C.    started 24-aug-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CHTAG*80, CHWYL*16
      COMMON /TAGC/  LLORG,LNORG,LLACT,LNACT, LALTN,LMODE,NCHTAG
     +,              CHTAG,CHWYL
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
      CHARACTER    MASK*4, MASKK(4)*4
      DATA  MASKK  / ' z  ', ' .z ', ' y  ', ' .y ' /

      IAND (IZV,IZW) = AND (IZV, IZW)


      LEV   = LEVEL
      IF (NCHTAG.LE.0)             GO TO 31
      IF (LMODE.LT.2)              GO TO 30

C--           Print pending tag

      CALL DPHEAD
      NQUSED = NQUSED + 1
      NQLLBL = 0

      JP = MIN (LMODE,3) - 1
      IF (LALTN.NE.0)  JP= JP + 2
      MASK = MASKK(JP)

      NBL = NQCMAX - NCHTAG - 26
      NBL = MIN (NBL,75)
      WRITE (IQPRNT,9024)  CHWYL,MASK,CQBLAN(1:NBL),CHTAG(1:NCHTAG)
 9024 FORMAT (A,A,'-void-',A,A)

   30 NCHTAG = 0
   31 IF (LEV.LT.0)                RETURN
      CHWYL  = ' '
      IF (IXKEEP.EQ.0)             GO TO 41
      CHTAG(1:10) = ' '

C--       construct prefix

      NUMORG = IAND (IQ(LNORG), KM19-1)
      CALL CSETDI (NUMORG, CHWYL,2,5)
      CALL CLEFT  (CHWYL,2,5)
      JP = NESLAT
      CHWYL(JP:JP) = '='
      JP = JP + 1

      IXORG = IQ(LNORG+1)
      IF (IXORG.EQ.0)  IXORG = IQ(LNORG+2)
      CALL NA_GET (IXORG,CHWYL,JP)

C--       construct tag

      IF (LEV.NE.0)  THEN
          CALL CSETDI (LEV+1,CHTAG,1,2)
          JP = 4
        ELSE
          JP = 1
        ENDIF

      CALL NA_GET (IXKEEP,CHTAG,JP)
      NCHTAG  = NESLAT - 1
      LLORG   = LNORG
      LALTN   = IFALTN
      LMODE   = 2 + LEV
      NQWYLDO = -1
      RETURN

C--       construct tag to signal continuation

   41 CHTAG(1:12) = '   continued'
      CALL CSETDI (LEV+1,CHTAG,1,2)
      NCHTAG  = 12
      LLORG   = 0
      LALTN   = IFALTN
      LMODE   = -2
      NQWYLDO = -3
      RETURN
      END
