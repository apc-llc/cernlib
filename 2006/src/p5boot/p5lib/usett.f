CDECK  ID>, USETT.
      SUBROUTINE USETT (JSL)

C-    Ready a title line for usage
C.    started 26-may-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      CHARACTER       CHTTNA*10, CHTTDT*20
      COMMON /USETTC/  JTTNAM,JTTVER,JTTSLA,JTTDAT,JTTCOM
     +,                NTTNA(5),  NTTDT,  NTTNORM, NTTALL
     +,               CHTTNA(5), CHTTDT
      PARAMETER      (NEWLN=10, NCHNEWL=1)
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
      PARAMETER      (NSIZETX=40*NSIZELN)
                     CHARACTER    TEXT(NSIZETX)*1
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ), MLIAD(NSIZELN)
                     EQUIVALENCE (LQ,IQ,LQGARB), (MLIAD(1),LQ(NSIZEQ))
                     EQUIVALENCE (TEXT(1), MLIAD(NSIZELN))
      COMMON //      IQUEST(100),LQGARB,LQHOLD,LQARRV,LQKEEP,LQPREP
C--------------    End CDE              --------------------------------
      CHARACTER    LINE*128,      KARD*128
      EQUIVALENCE (LINE,SLERRM), (KARD,SLLINE)

C-  PATCHY   5.00 /72  1994/05/24 21.00    text
C-  _:.=+=.: 1_:.=+=.: 2_:.=+=.: 3_:.=+=.: 4_:.=+=.: 5_:.=
C-
C-  start at col:
C-         JTTNAM   CHTTNA(1) =  name          PATCHY
C-         JTTVER   CHTTNA(2) =  version       5.00 /72
C-                  CHTTNA(3) =  numeric vs    50072
C-
C-                  CHTTNA(4) =  primary vs    5.00
C-         JTTSLA   CHTTNA(5) =  secondary vs  72
C-
C-         JTTDAT   CHTTDT    =  date/time     1994/05/24 21.00
C-         JTTCOM                comment
C-

      CALL VZERO (JTTNAM,12)
      CHTTNA(2) = '1.00'
      CHTTNA(3) = '10000'
      CHTTNA(4) = '1.00'
      CHTTNA(5) = '0'
      CHTTDT    = ' '

      NTTNA(2) = 4
      NTTNA(3) = 5
      NTTNA(4) = 4
      NTTNA(5) = 1
      NTTDT    = 0

      NVPRIM = 100
      NVSEC  = 0

      IF (JSL.GT.0)  THEN
          LINE = ' '
          CALL LN_GET (JSL, LINE, LEN(LINE))
          NTIT = NDSLAT
          CALL CLTOU (LINE(1:NTIT))
        ELSE
          LINE = 'unknown  /1'
          NTIT = LNBLNK (LINE(1:16))
        ENDIF

C--       check start with +TITLE: or C

      JTK = 1
      IF (LINE(1:4).EQ.'+TIT')  THEN
          J = ICFIND (':', LINE,5,NTIT)
          IF (NGSLAT.NE.0)  JTK= J + 1
        ELSEIF (LINE(1:2).EQ.'C ')  THEN
          JTK = 3
        ENDIF

C--       name

      JTK = ICNEXT (LINE,JTK,NTIT)
      N   = MIN (10,NDSLAT)
      JNX = NESLAT
      NTTNA(1)  = N
      CHTTNA(1) = LINE(JTK:JTK+N-1)
      JTTNAM = JTK
      IF (JSL.GT.0)  CALL LN_GET (JSL, LINE, LEN(LINE))

C--       does the version start with "/" ?

      JTK = ICNEXT (LINE,JNX,NTIT)
      IF (JTK.GE.NTIT)             RETURN

      N   = NDSLAT
      JNX = NESLAT
      IF (LINE(JTK:JTK).NE.'/')    GO TO 26
      JTTSLA = JTK

      N = MIN (4,N)
      KARD(1:N) = LINE(JTK:JTK+N-1)
      IF (N.EQ.1)  THEN
          KARD(2:2) = '1'
          N = 2
        ENDIF

      NTTNA(2) = N
      NTTNA(5) = N - 1
      CHTTNA(2) = KARD(1:N)
      CHTTNA(5) = KARD(2:N)
      NVSEC = ICDECI (KARD,2,N)
      GO TO 31

C--       primary version

   26 JTTVER = JTK
      JMK    = ICFIND ('/', LINE,JTK,JNX)
      IF (JMK.LT.JNX)  THEN
          JNX = JMK
          N   = JNX - JTK
        ENDIF

      N  = MIN (6,N)
      NTTNA(2) = N
      NTTNA(4) = N

      KARD(1:N) = LINE(JTK:JTK+N-1)
      CHTTNA(2) = KARD(1:N)
      CHTTNA(4) = KARD(1:N)

      NVPRIM = ICDECI (KARD,1,N) * 100
      JM = NESLAT + 1
      IF (JM.LE.N)  NVPRIM = NVPRIM + ICDECI(KARD,JM,N)

C--       secondary version

      JTK = ICNEXT (LINE,JNX,NTIT)
      N   = NDSLAT - 1
      JNX = NESLAT
      IF (LINE(JTK:JTK).NE.'/')    GO TO 34
      JTTSLA = JTK
      IF (N.LE.0)                  GO TO 31

      NTTNA(5)  = N
      CHTTNA(5) = LINE(JTK+1:JTK+N)
      NSEC = ICDECI (CHTTNA(5),1,N)

      NP = NTTNA(4)
      CHTTNA(2) = CHTTNA(4)(1:NP) // ' /' // CHTTNA(5)(1:N)
      NTTNA(2)  = LNBLNK (CHTTNA(2))

C--       PAM creation date + time

   31 JTK = ICNEXT (LINE,JNX,NTIT)
      JNX = NESLAT

   34 IF (JTK.GT.NTIT)                  GO TO 41
      IF (ICTYPE(LINE(JTK:JTK)).NE.2)   GO TO 41
      JTM = ICNEXT (LINE,JNX,NTIT)
      IF (JTM.LE.NTIT)                 THEN
      IF (ICTYPE(LINE(JTM:JTM)).EQ.2)  THEN
          JNX = NESLAT
        ENDIF
        ENDIF

      N  = JNX - JTK
      KARD(1:N) = LINE(JTK:JTK+N-1)
      CALL CSQMBL (KARD,1,N)
      N = MIN (N,20)
      N = LNBLNK (KARD(1:N))
      CHTTDT(1:N) = KARD(1:N)
      NTTDT = N
      JTK   = ICFNBL (LINE,JNX,NTIT)

C--       done

   41 NTTALL  = NTIT
      NTTNORM = NTIT
      IF (JTK.LE.NTIT)  THEN
          NTTNORM = LNBLNK (LINE(1:JTK-1))
          JTTCOM  = JTK
        ENDIF

      KARD(1:6) = ' '
      CALL CSETDI (100*NVPRIM+NVSEC, KARD,1,6)
      CALL CLEFT (KARD,1,6)
      NTTNA(3)  = NDSLAT
      CHTTNA(3) = KARD(1:6)
      RETURN
      END
