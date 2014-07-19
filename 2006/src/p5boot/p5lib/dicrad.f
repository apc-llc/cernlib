CDECK  ID>, DICRAD.
      SUBROUTINE DICRAD

C-    Write the Patchy update cradle
C.    started 6-june-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      PARAMETER (KM1=1,KM2=2,KM3=4,KM4=8,KM5=16,KM6=32,KM7=64,KM8=128,
     +  KM9=256, KM10=512, KM11=1024, KM12=2048, KM13=4096, KM14=8192,
     +  KM15=16384, KM16=32768, KM17=65536, KM18=131072, KM19=262144)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
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
     +,   INMEM(2),NSLTOT(2),NSLMAX(2),NTXMAX(2),JSLTTF(2), IFSTRUC
     +,   NOBJTT(3,2),NOBJMAT, MDELADD(2,2), LUNCRA,NLSENT,   LASTDI
      COMMON /DIFFC/ NFAID, JFAIDA, JFAIDE
C--------------    End CDE              --------------------------------
      CHARACTER    LNDUP*80, LINE*128, COL(128)*1, MARK*4
      EQUIVALENCE (LNDUP,SLLINE), (LINE,SLERRM), (COL,SLERRM)
      DIMENSION    MFAID(NSIZEQ)
      EQUIVALENCE (MFAID,IQ)
      LOGICAL      ALSO, PRINT

      CHARACTER    ACTION(3)*4
      DATA  ACTION / '+DEL', '+REP', '+ADD' /

      IAND (IZV,IZW) = AND (IZV, IZW)


      ALSO = IQTYPE .NE. IQPRNT
      CALL DPBLAN (1)

      IF (IXLUN(3).EQ.0)  THEN
          PRINT  = .TRUE.
          ASSIGN 9008 TO IFMT
          LUNCRA = IQPRNT
          WRITE (IQPRNT,9002)
          GO TO 20
        ENDIF

      PRINT  = .FALSE.
      ASSIGN 9009 TO IFMT
      LUNCRA = 21
      CALL FLINK (LUNCRA,5,IXLUN(3),0)
      WRITE (IQPRNT,9001) CHLIFI(1:NLIFI)
      IF (ALSO)  WRITE (IQTYPE,9001) CHLIFI(1:NLIFI)

 9001 FORMAT (' the Nypatchy update cradle goes to file ',A)
 9002 FORMAT (' Here comes the Nypatchy update cradle:'/)
 9004 FORMAT (A)
 9005 FORMAT ('+nil.  Attention: PAM structure changes !')
 9008 FORMAT (3X,A,'.')
 9009 FORMAT (A,'.')

C----              Write the leading lines to the cradle

      LINE = 'update  /1  ' // CQDATEM // ' ' // CQTIME
     +                      // '  from Nydiff'
      N    = LENOCC (LINE)
      WRITE (LUNCRA,9004) LINE(1:N)

C-            _:.=+=.: 1_:.=+=.:_2
      LINE = '+nil.  "old" was:'
      CALL LN_GET (JSLTTF(1),LINE(20:),40)
      N  = LENOCC (LINE)
      WRITE (LUNCRA,9004) LINE(1:N)

      LINE = '+nil.  "new" was:'
      CALL LN_GET (JSLTTF(2),LINE(20:),40)
      N  = LENOCC (LINE)
      WRITE (LUNCRA,9004) LINE(1:N)
      IF (IFSTRUC.NE.0)   WRITE (LUNCRA,9005)

      LINE = '+PATCH, UPD.'
      N    = 12
      IF (IXLUN(3).GT.0)  THEN
          CALL FLSPLIT (IXLUN(3),IXDIR,IXFN,IXEXT)
          IF (IXFN.GT.0)  THEN
              CALL NA_GET (IXFN,LINE,9)
              N = NESLAT
              COL(N) = '.'
            ENDIF
        ENDIF
      WRITE (LUNCRA,9004) LINE(1:N)

C--------     generate the +USE, T=INH lines for the "old" file  ------

C-            _:.=+=.: 1_:.=+=.:_2
   20 LINE = '+use, t=inh, p='
      JCOL = 16

      LFI = LQUSER(1)
      LP  = LFI - 3

C--           scan all patches

   22 LP = LQ(LP-1)
      IF (LP.EQ.0)                 GO TO 27
      IF (IAND(IQ(LP),KM2).NE.0)   GO TO 22
      CALL NA_GET (IQ(LP+4),LINE,JCOL)
      JCPE = NESLAT - 1
      IF (LQ(LP-2).NE.0)           GO TO 24
      WRITE (LUNCRA,IFMT) LINE(1:JCPE)
      NLSENT = NLSENT + 1
      MDELADD(1,1) = MDELADD(1,1) + 1
      IF (IQ(LP+10).EQ.0)          GO TO 22

      NQERR = NQERR + 1
      LNDUP = 'p='
      CALL NA_GET (IQ(LP+4),LNDUP,3)
      N = NESLAT
      WRITE (IQPRNT,9023) LNDUP(1:N)
      GO TO 22

 9023 FORMAT (' ***!!!  Error: deleting duplicate ',A,' !!!***')

C--           scan all decks of this patch

   24 LINE(JCPE+1:JCPE+4) = ', D='
      JCD = JCPE + 5

      LD = LQ(LP-4)
   25 LD = LQ(LD-1)
      IF (LD.EQ.0)                 GO TO 22
      IF (LQ(LD-2).NE.0)     GO TO 25
      CALL NA_GET (IQ(LD+4),LINE,JCD)
      WRITE (LUNCRA,IFMT) LINE(1:NESLAT-1)
      NLSENT = NLSENT + 1
      MDELADD(2,1) = MDELADD(2,1) + 1
      IF (IQ(LD+10).EQ.0)          GO TO 25

      NQERR = NQERR + 1
      LNDUP = 'p='
      CALL NA_GET (IQ(LP+4),LNDUP,3)
      N = NESLAT
      LNDUP(N+1:N+2) = 'd='
      CALL NA_GET (IQ(LD+4),LNDUP,N+3)
      N = NESLAT
      WRITE (IQPRNT,9023) LNDUP(1:N)
      GO TO 25

   27 IF (PRINT .AND. NLSENT.NE.0)  CALL DPBLAN (-1)

C--------     generate corrections for existing decks    -------------

      LFI = LQUSER(2)

C----         scan all patches

      LPN = LFI - 3
   32 LPN = LQ(LPN-1)
      IF (LPN.EQ.0)                GO TO 60
      LPX = LQ(LPN-2)
      IF (LPX.EQ.0)                GO TO 32
      CALL DINEED (2,LPN)

C----         scan all decks of this patch
C--           first take only the decks of the matching patch

      IXPUSE = 0
      LDNEXT = 0
      LDN = LPN - 3
   34 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                GO TO 57

      LDX  = LQ(LDN-2)
      IF (LDX.EQ.0)                GO TO 34
      IF (IQ(LDX+2).LE.0)          GO TO 34

C--       check the deck is of foreign origin
      IF (IQ(LDN+6).GE.4)  THEN
          LDNEXT = LDN
          GO TO 34
        ENDIF

C--       skip if done
      IF (IQ(LDX+6).EQ.4)          GO TO 34

C----         compare the "new" deck to the "old"

   41 IQ(LDX+6) = 4
      MARK = ' -> '
      CALL DINEED  (1,LPX)
      CALL DIF_XQT (IQ(LDX+1),IQ(LDX+2), IQ(LDN+1),IQ(LDN+2),0)
      IF (NFAID.EQ.0)              GO TO 49

      IF (IQ(LDX+10)+IQ(LDN+10) .NE.0)  THEN
          CALL DIACERR (LDX,IFERR)
          IF (IFERR.NE.0)          GO TO 49
        ENDIF

      IXP  = IQ(LPX+4)
      IXD  = IQ(LDX+4)
      LINE = '+xxx, '
      CALL NA_GET (IXP,LINE,7)
      JN = NESLAT
      COL(JN) = ','
      IF (IXD.EQ.0)  THEN
          JN = JN + 1
        ELSE
          CALL NA_GET (IXD,LINE,JN+2)
          JN = NESLAT
        ENDIF
      COL(JN) = ','
      JPUT = JN + 2

      JFAID = JFAIDA
   44 JCODE = MFAID(JFAID)
      JFGOX = MFAID(JFAID+1) - 1
      JFGON = MFAID(JFAID+2) - 1
      NLDEL = MFAID(JFAID+3)
      NLINS = MFAID(JFAID+4)
      IF (NLDEL+NLINS.EQ.0)        GO TO 47
      LINE(1:4) = ACTION(JCODE+2)
      LINE(JPUT:JPUT+12) = ' '

      IF (JCODE.GT.0)  THEN
          IF (JFGOX.EQ.0)  THEN
              LINE(1:4) = '+ADB'
            ELSE
              JFGOX = JFGOX - 1
            ENDIF
        ENDIF

      CALL CSETDI (JFGOX, LINE,JPUT,JPUT+5)
      IF (NLDEL.GE.2)  THEN
          COL(JPUT+6) = '-'
          CALL CSETDI (JFGOX+NLDEL-1, LINE,JPUT+7,JPUT+12)
        ENDIF
      CALL CLEFT (LINE,JPUT,JPUT+12)
      N = NESLAT - 1

      IF (PRINT)  WRITE(IQPRNT,9004) MARK
      WRITE (LUNCRA,IFMT) LINE(1:N)
      NLSENT = NLSENT + 1
      IF (NLINS.NE.0)  CALL DISEND (IQ(LDN+1)+JFGON, NLINS)
      MARK = ' >  '

   47 JFAID = JFAID + 5
      IF (JFAID.LT.JFAIDE)         GO TO 44
   49 IF (IXPUSE.EQ.0)             GO TO 34

C--           scanning foreign decks coming all from the same old patch

   52 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                GO TO 57
      LDX = LQ(LDN-2)
      IF (LDX.EQ.0)          GO TO 52
      IF (IQ(LDX+6).EQ.4)    GO TO 52
      IF (IQ(LDX+2).LE.0)    GO TO 52
      IF (IQ(LDN+6).EQ.IXPUSE)     GO TO 41
      LDNEXT = LDN
      GO TO 52

C--           start scanning foreign decks coming from patch IXPUSE

   57 IF (LDNEXT.EQ.0)             GO TO 32
      LDN = LDNEXT
      LDX = LQ(LDN-2)
      LPX = LQ(LDX-3)
      IXPUSE = IQ(LDN+6)
      LDNEXT = 0
      LDN    = LQ(LPN-4)
      GO TO 52

C--------     add new patches or decks      --------------------------

   60 LFI = LQUSER(2)

C----         scan all patches

      LPN = LFI - 3
   62 LPN = LQ(LPN-1)
      IF (LPN.EQ.0)                GO TO 87
      LPX = LQ(LPN-2)
      IF (LPX.EQ.0)                GO TO 71

C----         scan all decks of this patch

      LDN = LPN - 3
   64 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                GO TO 62
      LDX  = LQ(LDN-2)
      IF (LDX.EQ.0)                GO TO 72
      IF (IQ(LDX+9).LT.0)          GO TO 72

C--       check the deck is of foreign origin
      IF (IQ(LDN+6).GE.4)          GO TO 64

C--       remember this deck for adding to it
      LLD = LDX
      GO TO 64

C--           this patch is new, generate a +ADD for it

   71 LUSE = LPN
      J = 1
      GO TO 74

C--           this deck is new, generate a +ADD for it

   72 LUSE = LDN
      J = 2
      IF (IQ(LDN+4).EQ.0)  J= 1

C--       send the lines held by the bank at LUSE to the cradle

   74 MDELADD(J,2) = MDELADD(J,2) + 1
      CALL DINEED (2,LPN)
      LLP  = LQ(LLD-3)
      IXP  = IQ(LLP+4)
      IXD  = IQ(LLD+4)
      LINO = IQ(LLD+2)

      LINE = '+ADD, '
      CALL NA_GET (IXP,LINE,7)
      JN = NESLAT
      COL(JN) = ','
      IF (IXD.EQ.0)  THEN
          JN = JN + 1
        ELSE
          CALL NA_GET (IXD,LINE,JN+2)
          JN = NESLAT
        ENDIF
      COL(JN) = ','
      JN = JN + 2
      CALL CSETDI (LINO, LINE,JN,JN+5)
      CALL CLEFT (LINE,JN,JN+5)
      N = NESLAT - 1

      MARK = ' -> '
      IF (PRINT)  WRITE(IQPRNT,9004) MARK
      WRITE (LUNCRA,IFMT) LINE(1:N)
      NLSENT = NLSENT + 1
      CALL DISEND (IQ(LUSE+1),IQ(LUSE+2))
      IF (LPX.EQ.0)                GO TO 62
      GO TO 64

C----         Done

   87 IF (PRINT .AND. NLSENT.NE.0)  CALL DPBLAN (-1)
      WRITE (IQPRNT,9087) NLSENT
      IF (ALSO)  WRITE (IQTYPE,9087) NLSENT
      WRITE (IQPRNT,9088) MDELADD
      IF (ALSO)  WRITE (IQTYPE,9088) MDELADD
 9087 FORMAT (' the Nypatchy update cradle has',I6,' active lines,')
 9088 FORMAT (I6,' old patches,',I5,' old decks deleted,'
     F/       I6,' new patches,',I5,' new decks added.')
      NQLLBL = 0
      RETURN
      END
