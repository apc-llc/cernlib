CDECK  ID>, FAILCC.
      SUBROUTINE FAILCC (JSEVER,MSG)

C-    Print faulty control line pointed to by bank at LDOAN
C.    started 29-jan-92

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
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*(*)


      IF (LDOAN.EQ.0)              GO TO 24
      JSL = IQ(LDOAN+1)
      IF (LUPAN.EQ.0)  THEN
          NUM  = JSL - JSLORG
          LORG = 0
        ELSE
          NUM  = IQ(LDOAN+3)
          LORG = LQ(LDOAN-2)
        ENDIF
      CALL FAILLN (JSEVER,LORG,NUM,JSL,MSG)
      RETURN

C--           At end of deck

   24 CALL FAILLN (JSEVER,0,0,0,MSG)
      RETURN
      END
