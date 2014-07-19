CDECK  ID>, ARRNXD.
      SUBROUTINE ARRNXD (JSTEP)

C-    Set the limit of the next deck, find type of the deck after
C-        JSTEP = 1  step to next deck
C-                0  find the end of current deck only
C-               -1  determine also the type of the current deck
C.    started  5-mar-92

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


      JSLA = IQ(LQHOLD+1)
      JSLE = JSLA + IQ(LQHOLD+2)

C--       JSTEP > 0:  step to next deck

      IF (JSTEP.GT.0)  THEN
          IF (JDKNEX.EQ.-13)  CALL P_KILL ('Misplaced +MORE line')
          IF (JDKNEX.LE.0)    CALL P_CRASH ('trouble in ARRNXD')
          JDKTYP = JDKNEX
          JSLA   = JSLA + IQ(LQHOLD+3)
          IQ(LQHOLD+1) = JSLA
          IQ(LQHOLD+2) = JSLE - JSLA
          IF (JDKTYP.NE.6)         GO TO 24
          RETURN
        ENDIF

C--       JSTEP = <0: type of current deck

      IF (JSTEP.LT.0)  THEN
          CALL NEXTSI (JSLA,JDKTYP,INCRAD)
        ENDIF

C--       find end of current deck

   24 CALL NEXTDE (JSLA+1,JSLE,JSLF,JDKNEX,INCRAD)
      IQ(LQHOLD+3) = JSLF - JSLA

      IF (JDKNEX.NE.0)             RETURN

      IF (IQ(LARRV+8).NE.0)  JDKNEX= 6
      RETURN
      END
