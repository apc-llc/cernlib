CDECK  ID>, SMCODE.
      SUBROUTINE SMCODE (CODE)

C-    Crack CODE into JXTYP, JXACT, JXSTR
C.    started 26-jan-94

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      PARAMETER     (NTYSZ=4, NACSZ=4, NLNSZ=4,  NRTSZ=10000)
      CHARACTER      CHNLOG*80,    CODEX*8, CHTYP*8
      COMMON /SHMKC/ NNLOG,CHNLOG, CODEX,   CHTYP(17)
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
     +,    NLINST(4,NACSZ,NTYSZ), IXINST(NLNSZ,4,NACSZ,NTYSZ)
     +,    NCOUNTR(4,NTYSZ), NCSUMR(NTYSZ)
     +,    NCOUNTH(4,NTYSZ), NCSUMH(NTYSZ), IXOPTN(4,3), IXOPTP(4,3)
     +,    NRXQT, NROUT, NNAME(NRTSZ),  NRALLC, NRDEL
     +,    LUNSH, IXOBJ, IXSDIR,  JXTYP, JXACT, JXSTR,  LASTSH
C--------------    End CDE              --------------------------------
      CHARACTER    CODE*(*)
      PARAMETER   (NTYPES = 17)

C-   code   fo: fopt fc fort:   co: copt cc cc:   ao: aopt as as:
C-  CHTYP(    1    2  3     4     5    6  7   8     9   10 11  12
C-  JXTYP=    1                   2                 3
C-  JXACT=    1    2  3     4     1    2  3   4     1    2  3   4

C-   code   start: start end: end  sdir
C-  CHTYP(      13    14   15  16    17
C-  JXTYP=       4     4    4   4     5
C-  JXACT=       1     2    3   4

C--           if all blank: leave the parameters unchanged

      NA = LNBLNK (CODE)
      IF (NA.EQ.0)                 RETURN
      CODEX = CODE

C--           find the length of the string and compare

      NC  = ICFIND (':', CODEX,1,NA)
      IF (NC.EQ.1)                 GO TO 49

      JTYP = ICNTHL (CODEX(1:NC),CHTYP,NTYPES) - 1
      IF (JTYP.LT.0)               GO TO 49

      JXTYP = JTYP/4       + 1
      JXACT = MOD (JTYP,4) + 1
      JXSTR = 1
      IF (CODEX(NC:NC).EQ.':')     GO TO 24
      IF (JXTYP.LE.3)              RETURN

C--           take start:1 for start and end:1 for end

      JXACT = JXACT - 1
      RETURN

C--           get number from :n

   24 JXSTR = ICDECI (CODEX, NC+1, 8)
      IF (JXSTR.LE.0)              GO TO 49
      IF (JXSTR.LE.4)              RETURN
      IF (JXTYP.NE.4)              GO TO 49
      JXACT = JXACT + 1
      JXSTR = JXSTR - 4
      IF (JXSTR.LE.4)              RETURN

C--           CODEX not for one of the compilers

   49 JXTYP = 0
      END
