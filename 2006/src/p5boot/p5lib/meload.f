CDECK  ID>, MELOAD.
      SUBROUTINE MELOAD

C-    Load the "merge" file
C.    started 11-july-95

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /LUNSLN/IFLAUX, IXLUN(12)
      COMMON /MQCM/  NQSYSS,NQLINK, LQCSTA(3),LQCEND(3), NQMAX
     +,              LQLSTA(5),LQLEND(5), LQADR,LQADR0,NQOFFS
      PARAMETER (KQUSER=9)
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


      IF (IXLUN(1).EQ.0)           RETURN
      CALL INIPAM (11,IXLUN(1),1,0)
      CALL ARRIVE (1)
      IF (JDKTYP.GE.4)             GO TO 49

      LDECK  = KQUSER + 1
      NSLCUM = 0

   21 IF (JDKNEX.EQ.0)  CALL ARRIVE (2)

      JSLF = IQ(LQHOLD+1)
      NSLF = IQ(LQHOLD+3)
      IF (NSLCUM.EQ.0)  JSLFIR= JSLF

      CALL MQLIFT (LDECK, LDECK,-1,  JBKDEC,1)
      IQ(LDECK+1) = JSLF
      IQ(LDECK+2) = NSLF
      IQ(LDECK+3) = JDKTYP
      NSLCUM = NSLCUM + NSLF

C--           step to the next deck

      CALL ARRNXD (1)
      IF (JDKTYP.LE.3)             GO TO 21

      IF (JSLFIR+NSLCUM.NE.LQLEND(2))    GO TO 91

C--           reset division 1 to cover the "merge" material

      LQLEND(1) = LQLEND(2)
      LQLSTA(2) = LQLEND(2)

C--           close the input file

   49 CALL INIPAM (0,0,-1,0)
      RETURN

C-----        trouble, file too big

   91 CALL P_KILL ('MERGE file too big.')
      END
