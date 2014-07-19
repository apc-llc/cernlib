CDECK  ID>, DILOAD.
      SUBROUTINE DILOAD

C-    Load first the "new" and then the "old" file
C.    started 3-june-94

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      PARAMETER     (NSLIFI=128)
      CHARACTER      CHLIFI*(NSLIFI), CHLIEX*8
      COMMON /FLINKC/LUNOP,LUNFD,LUNOLD,LUNSIZ
     +,              IXFLUN, NLIFI,CHLIFI,CHLIEX
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
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


C-      On the VAX it is more important to keep file "new" in memory
C-      than file "old" because it will need more random access, hence:

C----         read file "new" into division 1, unit 12

      CALL INIPAM (12,IXLUN(2),1,0)
      CALL DILREAD (2)
      IF (NSLTOT(2).EQ.0)          GO TO 91
      LQLSTA(1) = LQLSTA(2)

C--           is file "new" complete in memory?

      IF (INMEM(2).EQ.0)           GO TO 27
      NSLFREE = LQLSTA(4)        - LQLEND(2)
      NTXFREE = MLIAD(LQLSTA(4)) - MLIAD(LQLEND(2))
      IF (MAX(NSLMAX(2),4000) .GE.NSLFREE)   GO TO 27
      IF (MAX(NTXMAX(2),16000).GE.NTXFREE)   GO TO 27
      LQLEND(1) = LQLEND(2)
      LQLSTA(2) = LQLEND(2)
      GO TO 31

C--           file "new" has to be handled in pieces

   27 CALL DICLFI (LQUSER(2))
      INMEM(2)  = 0
      LQLEND(1) = LQLSTA(1)
      LQLSTA(2) = LQLSTA(1) + NSLMAX(2) + 4
      LQLEND(2) = LQLSTA(2)
      MLIAD(LQLSTA(2)) = MLIAD(LQLSTA(1)) + NTXMAX(2) + 200

C----         read file "old" into division 2, unit 11

   31 CALL MQLIFT (LQHOLD, 0,7, JBKHOL,1)
      CALL MQLIFT (LARRV,LQARRV,1,JBKARR,3)
      CALL INIPAM (11,IXLUN(1),1,0)
      CALL DILREAD (1)
      IF (NSLTOT(1).EQ.0)          GO TO 91

C--           is file "old" complete in memory?

      IF (INMEM(1).NE.0)           GO TO 49
      CALL DICLFI (LQUSER(1))
      JSL1 = LQLSTA(1)
      JSL2 = LQLSTA(2)
      JSL4 = LQLSTA(4)
      JTX1 = MLIAD(JSL1)
      JTX2 = MLIAD(JSL2)
      JTX4 = MLIAD(JSL4)
      IF (INMEM(2).EQ.0)           GO TO 47

C-            if file "new" is in memory: is there enough space
C-            left for the longest patch on  "old" to be read back

      IF     (JSL2+NSLMAX(1)+8.LT.JSL4)  THEN
          IF (JTX2+NTXMAX(1)+8.LT.JTX4)  GO TO 48
        ENDIF
      CALL DICLFI (LQUSER(2))

C--           if both files are out of memory, divide the available
C--           space between them

   47 NSLGAP = JSL4 - JSL1 - NSLMAX(1) - NSLMAX(2)
      NTXGAP = JTX4 - JTX1 - NTXMAX(1) - NTXMAX(2)
      NSLGAP = MAX (4,NSLGAP/2)
      NTXGAP = MAX (4,NTXGAP/2)
      LQLSTA(2) =        JSL1 + NSLMAX(2) + NSLGAP
      MLIAD(LQLSTA(2)) = JTX1 + NTXMAX(2) + NTXGAP

   48 LQLEND(2) = LQLSTA(2)
   49 LQLSTA(3) = LQLSTA(4)
      LQLEND(3) = LQLSTA(3)
      RETURN

C----         Trouble

   91 WRITE (IQPRNT,9091) CHLIFI(1:NLIFI)
 9091 FORMAT (/' ***!!!  file ',A,' is empty  !!!***')
      CALL P_KILL ('fatal')
      END
