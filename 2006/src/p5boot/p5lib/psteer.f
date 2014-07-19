CDECK  ID>, PSTEER.
      SUBROUTINE PSTEER

C-    Running Patchy
C.    started  7-feb-92

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
     +,         LEXP,LLPAST,LQPAST, LQUSER(4), LHASM,LRPAM,LPAM, LQINCL
     +,         LACRAD,LARRV, LPCRA,LDCRAB, LEXD,LDECO, LCRP,LCRD, LSERV
     +, INCRAD, IFLGAR, JANSW, IFMODIF, IFALTN
     +, JDKNEX,JDKTYP, JSLZER,NSLORG,JSLORG
     +, MOPTIO(34), MOPUPD, NCLASH, IFLMERG,IFLDISP, NSLFRE,NTXFRE
     +, NVGAP(4), NVGARB(6), NVIMAT(4), NVUTY(4),  LASTWK
C--------------    End CDE              --------------------------------


      CALL PINIT (0)

C--       Get the cradle into memory

      INCRAD = 3
      CALL ARRIVE (0)

C--       Complete the initialization

      CALL PINIT (1)

C--       Do the cradle initially

      CALL DOCRAD
      IF (JANSW.NE.0)              GO TO 89

C--       Do next PAM file

   41 INCRAD = 0
      CALL DOPAM

C--       Do cradle continuation

      INCRAD = 2
      CALL DOCRAD
      IF (JANSW.EQ.0)              GO TO 41

   89 IF (NQERR.NE.0)        CALL PABEND
      CALL PEND
      RETURN
      END
