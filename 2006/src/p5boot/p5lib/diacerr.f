CDECK  ID>, DIACERR.
      SUBROUTINE DIACERR (LOLD,IFERR)

C-    Action on duplicate deck
C.    started 3-june-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*128, COL(128)*1
      EQUIVALENCE (LINE,SLERRM), (COL,SLERRM)


      LDX = LOLD
      LDN = LQ(LDX-2)
      LPX = LQ(LDX-3)
      LPN = LQ(LDN-3)

      LINE = 'p='
      CALL NA_GET (IQ(LPX+4),LINE,3)
      N = NESLAT
      LINE(N+1:N+2) = 'd='
      N = N + 3

      CALL NA_GET (IQ(LDX+4),LINE,N)
      N = NESLAT

      NDX = IQ(LDX+10)
      NDN = IQ(LDN+10)

      IF (NDN.NE.NDX)              GO TO 31
      IF (NDN.NE.1)                GO TO 31
      IF (IQ(LPX+4).NE.IQ(LPN+4))  GO TO 31

      NQWARN = NQWARN
      WRITE (IQPRNT,9024) LINE(1:N)
      IFERR = 0
      RETURN

   31 NQERR = NQERR
      WRITE (IQPRNT,9034) LINE(1:N)
      IFERR = 1
      RETURN

 9024 FORMAT (' **!!     Warn: Action on the first member of duplicate '
     F/16X,A,'     !!**')
 9034 FORMAT (' ***!!!  Error: Action on a bad member of duplicate '
     F/16X,A,'     !!!***')
      END
