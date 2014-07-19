CDECK  ID>, DPTAG.
      SUBROUTINE DPTAG (MODE,LEVEL,IXKEEP)

C-    Construct the next tail
C-      MODE  = zero:  normal P/D tag
C-                 1:  action line
C-            =    2:  KEEP tag
C-
C-     LEVEL  = 0, 1, 2, 3 ...  foreign-in-foreign level
C-
C-    IXKEEP    the name index of the sequence for MODE 2
C-              = 0 if material continued
C.    started 20-sept-93

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
      CHARACTER    MASK*4, MASKK(5)*4
      DATA  MASKK  / ' x- ', ' z- ', ' .z-', ' y- ', ' .y-' /

      IAND (IZV,IZW) = AND (IZV, IZW)


      MODEX = MODE
      LEV   = LEVEL
      IF (NCHTAG.LE.0)             GO TO 29
      IF (LMODE.LT.0)              GO TO 29

C--           Print pending tag

      J = 1
      IF (LMODE.GE.2)  THEN
          J = MIN (LMODE,3)
          IF (LALTN.NE.0)  J= J+2
        ENDIF
      MASK = MASKK(J)

      NBL = NQCMAX - NCHTAG - 10
      NBL = MIN (NBL,81)

      CALL DPHEAD
      NQUSED = NQUSED + 1
      NQLLBL = 0
      WRITE (IQPRNT,9024)  MASK,CQBLAN(1:NBL),CHTAG(1:NCHTAG)
 9024 FORMAT (6X,A,A,A)

      NCHTAG = 0
   29 NUMORG = IAND (IQ(LNORG), KM19-1)
      CHTAG(1:10) = ' '
      IF (MODEX.EQ.2)              GO TO 61

C--           Check same tag again

      IF (LNORG.EQ.0)              GO TO 49
      IF (LNORG.NE.LLORG)          GO TO 41
      IF (LNACT.NE.LLACT)          GO TO 41
      IF (LMODE.GE.2)              GO TO 41
      RETURN

C--           Ready new tag

C--           normal P/D tag
   41 IF (MODEX.EQ.0)  CALL CSETDI (LEV+1,CHTAG,1,1)

      CALL CSETDI (NUMORG, CHTAG,4,9)
      CALL CLEFT  (CHTAG,4,9)
      JP = NESLAT

C--       P/D tag

      IXPAT  = IQ(LNORG+1)
      IXDECK = IQ(LNORG+2)

      IF (LEV.NE.0)  THEN
          CHTAG(2:2) = 'x'
          GO TO 47
        ENDIF

      JTYPE = JBYT (IQ(LNACT),9,3)
      IF       (JTYPE.EQ.0)  THEN
          CHTAG(2:2) = 'D'
        ELSEIF (JTYPE.EQ.1)  THEN
          CHTAG(2:2) = 'R'
        ELSEIF (JTYPE.EQ.2)  THEN
          CHTAG(2:2) = 'B'
        ELSE
          CHTAG(2:2) = 'A'
        ENDIF

   47 CHTAG(JP:JP) = '='
      JP = JP + 1
      CALL NA_GET (IXPAT,CHTAG,JP)
      JP = NESLAT
      CHTAG(JP:JP) = '.'
      JP = JP + 1
      CALL NA_GET (IXDECK,CHTAG,JP)
      NCHTAG = NESLAT - 1
   49 LLORG = LNORG
      LLACT = LNACT
      LMODE = MODEX
      RETURN

C--       KEEP tag

   61 IF (LEV.NE.0)    CALL CSETDI (LEV+1,CHTAG,1,1)
      CHTAG(2:2) = 'Z'
      IF (IXKEEP.EQ.0)  THEN
          IF (LNORG.EQ.LLORG)  THEN
              IF (MODEX+LEV.EQ.LMODE)   GO TO 69
            ENDIF
          CHTAG(4:12) = 'continued'
          NCHTAG = 12
          GO TO 69
        ENDIF

      CALL CSETDI (NUMORG, CHTAG,4,9)
      CALL CLEFT  (CHTAG,4,9)
      JP = NESLAT
      CHTAG(JP:JP) = ':'
      JP = JP + 1
      CALL NA_GET (IXKEEP,CHTAG,JP)
      NCHTAG = NESLAT - 1
   69 LLORG  = LNORG
      LMODE  = MODEX + LEV
      LALTN  = IFALTN
      RETURN
      END
