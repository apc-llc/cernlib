CDECK  ID>, DIDUPL.
      SUBROUTINE DIDUPL (JFILE)

C-    Check presence of duplicate P/D names
C-    JFILE = 1/2 for old/new
C.    started 1-mar-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      CHARACTER      CQDATEM*10, CQDATE*8, CQTIME*5
      COMMON /QSTATE/NQERR,NQWARN,NQINFO,NQLOCK
     +,              IQDATE,IQTIME, CQDATEM,CQDATE,CQTIME
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER (KQUSER=9)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*80
      EQUIVALENCE (LINE,SLERRM)


      JFI = JFILE
      CALL DPBLAN (0)

      LFA = KQUSER + JFI
      LFA = LQ(LFA-1)
      LPA = LFA - 3

C--       next patch A
   22 LPA = LQ(LPA-1)
      IF (LPA.EQ.0)                RETURN
      IXP = IQ(LPA+4)
      LDA = LPA - 3

C--       next deck A
   24 LDA = LQ(LDA-1)
      IF (LDA.EQ.0)                GO TO 22
      IXD = IQ(LDA+4)

C----         search for next deck of same name

      LPN = LPA
      LDN = LDA
   32 LDN = LQ(LDN-1)
      IF (LDN.EQ.0)                GO TO 37
      IF (IQ(LDN+4).EQ.IXD)        GO TO 41
      GO TO 32

   37 LPN = LQ(LPN-1)
      IF (LPN.EQ.0)                GO TO 24
      IF (IQ(LPN+4).NE.IXP)        GO TO 37
      LDN = LPN - 3
      GO TO 32

C--           trouble

   41 IQ(LDA+10) = MAX (1, IQ(LDA+10))
      IQ(LDN+10) = IQ(LDA+10) + 1

      IF (JFI.EQ.1)  THEN
          LINE = 'old p='
        ELSE
          LINE = 'new p='
        ENDIF
      CALL NA_GET (IXP,LINE,7)
      N = NESLAT
      IF (IXD.EQ.0)  THEN
          IQ(LPA) = IQ(LDA+10)
          IQ(LPN) = IQ(LDN+10)
        ELSE
          LINE(N+1:N+2) = 'd='
          N = N + 3
          CALL NA_GET (IXD,LINE,N)
          N = NESLAT
        ENDIF

      WRITE (IQPRNT,9044) LINE(1:N)
      NQLLBL = 0
      NQINFO = NQINFO + 1
      GO TO 24

 9044 FORMAT (' *!  Duplicate: ',A,' !*')
      END
