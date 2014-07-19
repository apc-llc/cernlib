CDECK  ID>, PINIT.
      SUBROUTINE PINIT (JSTAGE)

C-    Initialize PATCHY
C.    started 10-feb-92

      PARAMETER (NBANKS=19,JBKPAT=1, JBKDEC=2, JBKORG=3, JBKINC=4,
     +           JBKHOL=5, JBKKEE=6, JBKACT=7, JBKMAT=8, JBKXSQ=9,
     +           JBKPRE=10,JBKGAR=11,JBKSMH=12,JBKSMT=13,JBKSML=14,
     +           JBKSMX=15,JBKARR=16,JBKASA=17,JBKPAM=18,JBKRPA=19)
      COMMON /QBANKS/MMBANK(5,NBANKS)
      COMMON /QUNIT/ IQREAD,IQPRNT, IQTTIN,IQTYPE, IQOFFL,IQRTTY,IQRSAV
     +,              IQRFD,IQRRD,IQRSIZ, NQLPAT,NQUSED,NQLLBL, NQINIT
      PARAMETER      (NFIMAX=100)
      COMMON /TITLEC/ NFILET, JTIPAM(NFIMAX)
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


      IF (JSTAGE.NE.0)             GO TO 31

C--------    Start initialization before reading the cradle   --------

      CALL MQWORK (LACRAD,INCRAD,LASTWK)
      NFILET = 0

      NVGAP(1) = 4000
      NVGAP(2) =  200
      NVGAP(3) = 5000
      NVGAP(4) =   40

C--       Pre-lift the banks for I/O handling

      CALL MQLIFT (LACRAD, 0,7, JBKARR,3)
      CALL MQLIFT (LPAM,   0,7, JBKPAM,3)
      CALL MQLIFT (LQARRV, 0,7, JBKARR,3)
      CALL MQLIFT (LQHOLD, 0,7, JBKHOL,1)
      RETURN

C--------    Finish initialization with the cradle in memory  -------

C--       preset options COMPACT FULL VERBOSE

   31 MOPTIO(2) = 1
      MOPTIO(3) = 1
      MOPTIO(6) = 1
      IF (IQTYPE.NE.IQPRNT)  MOPTIO(22)= 1
      CALL PKBYT (MOPTIO(1),MOPTIO(33),1,32,0)

      CALL PINIT2

C--       Ready the ASM structure and connect pre-opened files

      CALL ASMINI
      RETURN
      END
