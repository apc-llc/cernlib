CDECK  ID>, DQCLOBB.
      SUBROUTINE DQCLOBB (LNAME,LSTAT)

C-    Bank chaining clobbered has been detected at LNAME or LSTAT
C-    scan all divisions and print the last 4 good banks

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      COMMON /MQCN/  IQLST,IQLNA,IQID,IQTY,IQLI,IQNL,IQNS,IQND
     +,              IQLNX,IQFOUL
      PARAMETER      (NSIZEQ=100000, NSIZELN=100000)
                     DIMENSION    LQ(NSIZEQ), IQ(NSIZEQ)
                     EQUIVALENCE (LQ,IQ,LQGARB)
      COMMON //      IQUEST(100),LQGARB
C--------------    End CDE              --------------------------------
      DIMENSION    LLAST(4)


      WRITE (IQPRNT,9000)
      IF (LNAME.NE.0)  THEN
          WRITE (IQPRNT,9001) 'name',LNAME
        ELSE
          WRITE (IQPRNT,9001) 'status',LSTAT
        ENDIF

 9000 FORMAT (/' ****!!!!!   Patchy is crashing   !!!!!****'
     F/' please call for help: zoll@cern.ch')
 9001 FORMAT (/' Bank chaining clobbered at ',A,' adr',I7)

C----         Next division

      JDIV = 0
   21 JDIV = JDIV + 1
      IF (JDIV.GT.3)               GO TO 99

      LNA  = LQCSTA(JDIV)
      LEND = LQCEND(JDIV)

      WRITE (IQPRNT,9022) JDIV,LNA,LEND
 9022 FORMAT (/' Division',I2,' boundaries:',2I7)

C----         Scan to the trouble splot

      CALL VZERO (LLAST,4)

   31 IF (LNA.GE.LEND)             GO TO 21
      CALL QBLOWX (LNA)
      IF (IQFOUL.NE.0)             GO TO 41

      CALL UCOPY (LLAST(2),LLAST(1),3)
      LLAST(4) = LNA
      LNA = IQLNX
      GO TO 31

C----         Trouble spot found, dump the last 4 banks

   41 LNATR = LNA
      WRITE (IQPRNT,9041)
 9041 FORMAT (/' After:')

      JBK = 0
   42 JBK = JBK + 1
      IF (JBK.GT.4)                GO TO 44
      LNA = LLAST(JBK)
      IF (LNA.EQ.0)                GO TO 42

      CALL QBLOWX (LNA)
      CALL DQBANK
      GO TO 42

C----         Find the first good bank after

   44 LOK = 0
      LNA = LNATR
   45 LNA = LNA + 1
      IF (LNA.GE.LEND-4)           GO TO 46
      CALL QBLOWX (LNA)
      IF (IQFOUL.NE.0)             GO TO 45
      LOK = LNA

   46 LNAE = MIN (LNA, LNATR+39)

      WRITE (IQPRNT,9046)
 9046 FORMAT (/' the memory is destroyed, dump the next few words:'/)
 9047 FORMAT (1X,I8,' :',Z10,I20)

      DO 47 L=LNATR,LNAE
      WRITE (IQPRNT,9047) L,LQ(L),LQ(L)
   47 CONTINUE
      IF (LOK.EQ.0)                GO TO 21

      WRITE (IQPRNT,9049) LOK
 9049 FORMAT (/' The first good bank after, starting at adr',I8,' is:')

      CALL DQBANK
      GO TO 21

C----         Exit

   99 CALL P_KILL ('fatal')
      END
