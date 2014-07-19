CDECK  ID>, DOMAPA.
      SUBROUTINE DOMAPA (IFLACT)

C-    Content analysis of the next deck for the Auxiliaries,
C-    map the description of this deck's material
C-    into the control-banks of the linear PREP structure.
C-    Merge actions if IFLACT not zero
C.    started  12-jan-94

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
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



      JSLZER = IQ(LQHOLD+1)
      JSLORG = JSLZER

C--                 create the PREP seedling

      CALL MQLIFT (LQPREP, 0,7, JBKPRE,2)

      IQ(LQPREP+1) = JSLZER
      IQ(LQPREP+2) = IQ(LQHOLD+3)

C----     stage 1 : delimit control lines, create PREP structure

      CALL M_ANA1

      IF (IFLACT.EQ.0)             RETURN
      IF (LEXD.EQ.0)               RETURN
      IF (LQ(LEXD-2).EQ.0)         RETURN

C----     stage 2 : split PREP banks on action limits

      LDO  = LQPREP
      LACT = LEXD - 1

C--              Next action

   21 LACT = LQ(LACT-1)
      IF (LACT.EQ.0)               RETURN

C-                    JACT = 0 DEL, 1 REP, 2 ADB, 3 ADD

      JACT = JBYT (IQ(LACT),9,3)
      JSLX = IQ(LACT+4) + JSLZER

C-        break at the insertion point

      LDO = M_SPLIT (LDO,JSLX)
      IF (LDO.EQ.0)                GO TO 61

C-        zero-line PREP bank with reference to ACT

      LX  = LDO
      LDO = M_SPLIT (LDO,0)
      LQ(LX-2) = LACT
      GO TO 21

C--              Add trailing actions

   61 LNEW = KQLAST (LQPREP-1) + 1
   62 CALL MQLIFT (LNEW,LNEW,-1,JBKPRE,2)
      LQ(LNEW-2) = LACT

      LACT = LQ(LACT-1)
      IF (LACT.NE.0)               GO TO 62
      RETURN
      END
