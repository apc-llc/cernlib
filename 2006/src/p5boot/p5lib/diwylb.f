CDECK  ID>, DIWYLB.
      SUBROUTINE DIWYLB

C-    Make the Wylbur cradle
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
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*128, COL(128)*1
      EQUIVALENCE (LINE,SLERRM), (COL,SLERRM)
      LOGICAL      ALSO, PRINT

      IAND (IZV,IZW) = AND (IZV, IZW)


      ALSO = IQTYPE .NE. IQPRNT
      CALL DPBLAN (1)
      NLSENT = 0

      IF (IXLUN(3).EQ.0)  THEN
          PRINT  = .TRUE.
          ASSIGN 9008 TO IFMT
          LUNCRA = IQPRNT
          WRITE (IQPRNT,9002)
          GO TO 20
        ENDIF

      CALL NA_GET (IXLUN(3),LINE,1)
      N = NDSLAT
      J = ICFILA ('.', LINE,1,N)
      LINE(J:J+4) = '.uexe'
      N = J + 4
      IXFLUN = NA_LONG (LINE(1:N))

      PRINT  = .FALSE.
      ASSIGN 9009 TO IFMT
      LUNCRA = 21
      CALL FLINK (LUNCRA,5,-1,0)
      WRITE (IQPRNT,9001) CHLIFI(1:NLIFI)
      IF (ALSO)  WRITE (IQTYPE,9001) CHLIFI(1:NLIFI)

      LINE = '# update Wylbur Exec  ' // CQDATEM // ' ' // CQTIME
      N    = LENOCC (LINE)
      WRITE (LUNCRA,9004) LINE(1:N)

C-            _:.=+=.: 1_:.=+=.:_2
      LINE = '# "old" was:'
      CALL LN_GET (JSLTTF(1),LINE(15:),40)
      N  = LENOCC (LINE)
      WRITE (LUNCRA,9004) LINE(1:N)

      LINE = '# "new" was:'
      CALL LN_GET (JSLTTF(2),LINE(15:),40)
      N  = LENOCC (LINE)
      WRITE (LUNCRA,9004) LINE(1:N)

 9001 FORMAT (' the Wylbur update exec goes to file ',A)
 9002 FORMAT (' Here comes the Wylbur update exec:')
 9004 FORMAT (A)
 9008 FORMAT (3X,A)
 9009 FORMAT (A)

C------       generate the re-ordering commands

   20 LFI = LQUSER(2)

C--           re-order the patches of this PAM

      IF (IAND(IQ(LFI),KM3).EQ.0)  THEN
          CALL DIWYLR (LFI,1)
        ENDIF

C--           scan all patches

      LP = LFI - 3
   22 LP = LQ(LP-1)
      IF (LP.EQ.0)                 GO TO 27
      IF (IAND(IQ(LP),KM4).EQ.0)   GO TO 26

C--           foreign decks moved into this patch

      IXP = IQ(LP+4)
      LD  = LQ(LP-4)
   24 LL = LD
      LD = LQ(LL-1)
      IF (LD.EQ.0)                 GO TO 26
      IXPGET = IQ(LD+6) - 4
      IF (IXPGET.LT.0)       GO TO 24

      IF (PRINT)  WRITE (IQPRNT,9004)
      WRITE (LUNCRA,IFMT) '#     fetch a lost child'

C-            _:.=+=.:_1
      LINE = 'range p='
      CALL NA_GET (IXP,LINE,9)
      JC = NESLAT
      LINE(JC:JC+2) = ',d='
      CALL NA_GET (IQ(LL+4),LINE,JC+3)
      WRITE (LUNCRA,IFMT) LINE(1:NESLAT-1)

C-            _:.=+=.:_1_:.=+=.:_2
      LINE = 'move,n,f    |p='
      CALL NA_GET (IXPGET,LINE,16)
      JC = NESLAT
      LINE(JC:JC+2) = ',d='
      CALL NA_GET (IQ(LD+4),LINE,JC+3)
      WRITE (LUNCRA,IFMT) LINE(1:NESLAT-1)
C!    WRITE (LUNCRA,IFMT) 'num -nol'
      NLSENT = NLSENT + 3
      GO TO 24

   26 IF (IAND(IQ(LP),KM3).NE.0)   GO TO 22
      CALL DIWYLR (LP,0)
      GO TO 22

   27 IF (PRINT .AND. NLSENT.NE.0)  CALL DPBLAN (-1)
      WRITE (IQPRNT,9070) NLSENT
      IF (ALSO)  WRITE (IQTYPE,9070) NLSENT
 9070 FORMAT (' the Wylbur update exec has',I4,' active lines.')
      NQLLBL = 0

      RETURN
      END
