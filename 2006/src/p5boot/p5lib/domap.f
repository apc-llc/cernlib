CDECK  ID>, DOMAP.
      SUBROUTINE DOMAP

C-    Content analysis of the next deck,
C-    map the description of this deck's material
C-    into the control-banks of the linear PREP structure.
C.    started  9-jan-92

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
      COMMON /INCLC/ N_INCL, IX_INCL(100)
      COMMON /M_ANAC/LOWAN,KDOAN,LDOAN,LUPAN,MODEAN,MEXAN,LEVAN,KKM5AN
     +,              NEWDEC,NEWCTL,NEWFOR,NEWNIL,NEWINC
C--------------    End CDE              --------------------------------


      NEWDEC = 0
      NEWCTL = 0
      NEWFOR = 0
      NEWNIL = 0
      NEWINC = 0
      MEXAN  = 0
      N_INCL = 0
      JSLZER = IQ(LQHOLD+1)
      JSLORG = JSLZER - NSLORG

C--                 create the PREP seedling

      CALL MQLIFT (LQPREP, 0,7, JBKPRE,2)

      IQ(LQPREP+1) = JSLZER
      IQ(LQPREP+2) = IQ(LQHOLD+3)

C----     stage 1 : delimit control lines, create PREP structure

      IF (MOPUPD.GE.0)  CALL M_ANA1


C----     stage 2 : connect actions into this deck

      IF (LEXD.EQ.0)               GO TO 31
      IF (LQ(LEXD-2).EQ.0)         GO TO 31

      CALL M_ANA2

C--                 create NIL sequences or actions

      IF (NEWNIL.NE.0)  CALL MK_NIL


C----     stage 3 : ready effective self-material and new foreign material
C-        return JANSW =  -1 skip patch,  0 skip deck,  1 process

   31 CALL M_HEAD
      IF (JANSW.GT.0)  CALL M_ANA3
      RETURN
      END
