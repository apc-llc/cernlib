CDECK  ID>, DPLINE.
      SUBROUTINE DPLINE (LINUM,MASK,NCHL,LINE)

C-    Print one line for LIST
C-    LINUM is the line-number, not printed if -ve,
C-    MASK  is the line-type identifier
C-    LINE  of length NCHL is the text, void if NCHL <= 0
C.    started 5-aug-93

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
C--------------    End CDE              --------------------------------
      CHARACTER    CHNUM*6, MASK*4, LINE*512


      IF (NQUSED.GE.NQLMAX)  CALL DPPAGE
      IF (NQWYLDO.NE.0)            GO TO 41

C------       Without Wylbur line-numbers

      IF (NCHTAG.NE.0)  THEN
          IF (92+NCHTAG.GE.NQCMAX) GO TO 31
        ENDIF
   21 NQUSED = NQUSED + 1
      NQLLBL = 0

C-             _:.=+=
      CHNUM = '      '
      IF (LINUM.GE.0)  CALL CSETDI (LINUM,CHNUM,2,6)

C--           LINE is void

      IF (NCHL .GT.0)              GO TO 25
      IF (NCHTAG.EQ.0)  THEN
          WRITE (IQPRNT,9021) CHNUM,MASK
        ELSE
          WRITE (IQPRNT,9022) CHNUM,MASK,CHTAG(1:NCHTAG)
          NCHTAG = 0
        ENDIF
      RETURN

C--           LINE is present

   25 JE = NCHL
      IF (JE.GT.80)  JE= ICFILA(' ', LINE,60,80) - 1
      IF (NCHTAG.EQ.0)   THEN
          WRITE (IQPRNT,9025) CHNUM,MASK,LINE(1:JE)
        ELSE
          WRITE (IQPRNT,9026) CHNUM,MASK,LINE(1:JE),CHTAG(1:NCHTAG)
          NCHTAG = 0
        ENDIF
      IF (JE.EQ.NCHL)              RETURN

C--           LINE is longer than 80 characters

   28 JA  = JE + 1
      JE  = NCHL
      IF (JE-JA.GE.72)  JE= ICFILA(' ', LINE,JA+51,JA+71) - 1
      WRITE (IQPRNT,9028) LINE(JA:JE)
      NQUSED = NQUSED + 1
      IF (JE.LT.NCHL)              GO TO 28
      RETURN

C--           Print the tag stand-alone, if too long

   31 IF (NQUSED.GE.NQLTOK)  CALL DPPAGE
      NBL = NQCMAX - NCHTAG
      WRITE (IQPRNT,9025) CQBLAN(1:NBL), CHTAG(1:NCHTAG)
      NQUSED = NQUSED + 1
      NCHTAG = 0
      IF (NQWYLDO.EQ.0)            GO TO 21

C------       With Wylbur line-numbers

   41 IF (NCHTAG.NE.0)  THEN
          IF (102+NCHTAG.GE.NQCMAX)    GO TO 31
        ENDIF
      NQUSED = NQUSED + 1
      NQLLBL = 0

      IF (NQWYLDO.LT.0)  THEN
          IF (NQWYLDO.LT.-2)       GO TO 43
          IF (NQWYLDO.EQ.-2)  CHWYL = ' '
          NQWYLDO = NQWYLDO - 1
          GO TO 43
        ENDIF

      IF (LINUM.LT.0)  THEN
          CHWYL = ' '
          GO TO 43
        ENDIF

C-             _:.=+=.: 1_:.=+=.: 2
C-                123456. 12345
      CHWYL = '         .      '
      NQWYL = NQWYL + 1
      CALL CSETDI (NQWYL,CHWYL,2,9)
      CALL CSETDI (LINUM,CHWYL,11,16)

C--           LINE is void

   43 IF (NCHL .GT.0)              GO TO 45
      IF (NCHTAG.EQ.0)  THEN
          WRITE (IQPRNT,9021) CHWYL,MASK
        ELSE
          WRITE (IQPRNT,9023) CHWYL,MASK,CHTAG(1:NCHTAG)
          NCHTAG = 0
        ENDIF
      RETURN

C--           LINE is present

   45 JE = NCHL
      IF (JE.GT.80)  JE= ICFILA(' ', LINE,60,80) - 1
      IF (NCHTAG.EQ.0)   THEN
          WRITE (IQPRNT,9025) CHWYL,MASK,LINE(1:JE)
        ELSE
          WRITE (IQPRNT,9027) CHWYL,MASK,LINE(1:JE),CHTAG(1:NCHTAG)
          NCHTAG = 0
        ENDIF
      IF (JE.EQ.NCHL)              RETURN

C--           LINE is longer than 80 characters

   48 JA  = JE + 1
      JE  = NCHL
      IF (JE-JA.GE.72)  JE= ICFILA(' ', LINE,JA+51,JA+71) - 1
      WRITE (IQPRNT,9029) LINE(JA:JE)
      NQUSED = NQUSED + 1
      IF (JE.LT.NCHL)              GO TO 48
      RETURN

 9021 FORMAT (A,A)
 9022 FORMAT (A,A,T92,A)
 9023 FORMAT (A,A,T102,A)
 9025 FORMAT (A,A,A)
 9026 FORMAT (A,A,A,T92,A)
 9027 FORMAT (A,A,A,T102,A)
 9028 FORMAT (17X,'>',A)
 9029 FORMAT (27X,'>',A)
      END
