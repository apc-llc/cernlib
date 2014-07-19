CDECK  ID>, MQSHOW.
      SUBROUTINE MQSHOW

C-    Display current memory occupation

      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
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
C--------------    End CDE              --------------------------------


      CALL DPBLAN (0)
      WRITE (IQPRNT,9010)
      WRITE (IQPRNT,9011)  (LQCSTA(J),LQCEND(J),J=1,3)
      WRITE (IQPRNT,9012)  (LQLSTA(J),LQLEND(J),J=1,5)
      WRITE (IQPRNT,9013)  (MLIAD(LQLSTA(J)),MLIAD(LQLEND(J)),J=1,5)
      WRITE (IQPRNT,9014)  NVGAP
      NQUSED = NQUSED + 25
      RETURN

 9010 FORMAT (' Present state of memory,',
     F' start + end adrs of divisions:')
 9011 FORMAT (
     F/' control div 1: collectable foreign material',2I9
     F/' control div 2: processing current deck     ',2I9
     F/' control div 3: permanent foreign material  ',2I9)
 9012 FORMAT (
     F/' line slots of text div 1: foreign material      ',2I9
     F/' line slots of text div 2: PAM file in store     ',2I9
     F/' line slots of text div 3: volatile construction ',2I9
     F/' line slots of text div 4: permanent construction',2I9
     F/' line slots of text div 5: naming stack          ',2I9)
 9013 FORMAT (
     F/' text div 1: foreign material      ',2I9
     F/' text div 2: PAM file in store     ',2I9
     F/' text div 3: volatile construction ',2I9
     F/' text div 4: permanent construction',2I9
     F/' text div 5: naming stack          ',2I9)
 9014 FORMAT (/' Gap parameters to control garbage collection:'
     F/' Min. number of words free for control division 2',I7
     F/' Min. number of lines free for text divisions 3+4',I7
     F/' Max. number of lines expected in any deck       ',I7
     F/' Mean number of characters per line              ',I7/)
      END
