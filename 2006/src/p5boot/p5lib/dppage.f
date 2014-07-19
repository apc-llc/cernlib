CDECK  ID>, DPPAGE.
      SUBROUTINE DPPAGE

C-    LIST current deck/patch identifier, with or without page eject
C.    started 10-sep-93

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      CHARACTER      CQBLAN*128, CQEQS*128, CQCETA*32
      CHARACTER      CQAPO*1, CQBSL*1, CQTAB*1
      COMMON /QCHAR/ CQBLAN, CQEQS, CQCETA, CQAPO, CQBSL, CQTAB
      COMMON /QPAGE/ NQLMAX,NQLTOL,NQLTOK,NQCMAX,NQCPGH,NQPAGE
     +,              NQWYLDO,NQWYL,NQNEWH,NQJOIN,NQDKNO,NQDKPG
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      CHARACTER      CHEXPD*68
      COMMON /CHEXC/ IXEXPAM, IXEXPAT,IXEXDEC,IXEXID, NCHEPD, CHEXPD
C--------------    End CDE              --------------------------------
      CHARACTER    TAIL*13
      EQUIVALENCE (TAIL, SLLINE(161:))


      NQJOIN = 0
      NQLPAT = 0
      IF (NQUSED.GT.NQLMAX)  THEN
          NQUSED = NQUSED - NQLMAX
          RETURN
        ENDIF

      IF (NQUSED.LT.NQLTOL)  THEN
          NEWPAG = 0
        ELSE
          NEWPAG = 1
        ENDIF

      NCOL   = NQCPGH
      SLLINE(1:176) = ' '

C--       construct left edge:  167 PATCHY

      CALL CSETDI (NQDKNO,  SLLINE, 2,6)
      CALL CLEFT  (SLLINE, 3,6)
      J  = NESLAT + 1
      IF (IXEXPAM.NE.0)  THEN
          CALL NA_GET (IXEXPAM, SLLINE(1:J+7), J)
          JEQ = NESLAT + 2
        ELSE
          SLLINE(J:J+5) ='cradle'
          JEQ = J + 8
        ENDIF

C--      prepare right edge:  P=pname  dname  1 === pg 123
C-                                           _:.=+=.: 1_:.
      NQDKPG = NQDKPG + 1
      CALL CSETDI (NQDKPG, TAIL, 1,2)

      JID = NCOL - 13 - NCHEPD
      SLLINE(JID:JID+NCHEPD-1) = CHEXPD(1:NCHEPD)

      IF (NQNEWH.EQ.0)             GO TO 27

C--       starting a new deck

      NEQ = JID - JEQ - 2
      IF (NEQ.GT.0)   SLLINE(JEQ:JEQ+NEQ-1)= CQEQS(1:NEQ)

      IF (NEWPAG.EQ.0)  THEN
          TAIL(4:13)= CQEQS(1:10)
        ELSE
          TAIL(4:6)= CQEQS(1:3)
        ENDIF

C--       page eject?

   27 IF (NEWPAG.EQ.0)             GO TO 31
      SLLINE(1:1) = '1'
      NQPAGE = NQPAGE + 1

      CALL CSETDI (NQPAGE, TAIL,11,13)
      J = NFSLAT - 2
      TAIL(J:J+1) = 'pg'
      SLLINE(NCOL-12:NCOL) = TAIL
      WRITE (IQPRNT,9028) SLLINE(1:NCOL)
      NQUSED = 2
      NQLLBL = 1
      RETURN

   31 CALL DPBLAN (0)
      SLLINE(NCOL-12:NCOL) = TAIL
      WRITE (IQPRNT,9028) SLLINE(1:NCOL)
      NQUSED = NQUSED + 2
      RETURN

 9028 FORMAT (A/)
      END
