*
* $Id: fatexam1.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatexam1.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*======================================================================
*
*   Example FATMEN Program
*======================================================================
*
*     KEYS vector
*
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      COMMON/QUEST/IQUEST(100)
*
*     Generic name
*
      CHARACTER*255 GENAME
*
*     Units on which FATMEN will be read (LUNRZ) and
*     on which updates, if any, will be written (LUNFZ)
*
      LUNRZ = 73
      LUNFZ = 12
*
*     Initialise FATMEN and Zebra the easy way...
*
      CALL FMSTRT(LUNRZ,LUNFZ,'//CERN/OPAL',IRC)
 
      GENAME = '//CERN/OPAL/DDST/PASS3/FYZ1/P18R1929/C01'
      LG = LENOCC(GENAME)
*
*     Get this entry from the catalogue
*
      LBANK = 0
      CALL FMGET(GENAME(1:LG),LBANK,KEYS,IRC)
*
*     and display it using FMSHOW...
*     just the KEYS vector (option K)
*
      CALL FMSHOW(GENAME(1:LG),LBANK,KEYS,'K',IRC)
*
*     Comment field (C) full generic name (G) and TMS info (Q)
*
      CALL FMSHOW(GENAME(1:LG),LBANK,KEYS,'CGQ',IRC)
*
*     everything... (except options Q & Z)
*
      CALL FMSHOW(GENAME(1:LG),LBANK,KEYS,'A',IRC)
*
*     now we want to try and read the file...
*     open for read (R) and issue FZFILE (F) on unit 11
*     Put LBANK to 0 first. This will cause FMOPEN to
*     re-retrieve the information from the catalogue.
*     (We should really save LBANK in a ZEBRA link area)
*
      LBANK = 0
      IQUEST(10) = 0
      CALL FMOPEN(GENAME(1:LG),
     +'11',LBANK,'RF',IRC)
      IF(IRC.NE.0) THEN
         PRINT *,'Return code ',IRC,' from FMOPEN'
         GOTO 10
      ELSE
         CALL READFZ(11)
      ENDIF
*
*     Now clsoe the file. E=call FZENDx, D=Drop staging disk,
*     dismount tape, deassign logical name etc. as appropriate
*     Z=drop bank at LBANK
*
      CALL FMCLOS(GENAME,
     +                   '11',LBANK,'EDZ',IRC)
      IF(IRC.NE.0) PRINT *,'Return code ',IRC,' from FMCLOS'
1     CONTINUE
10    CONTINUE
*
      END
 
      SUBROUTINE READFZ(LUN)
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
      COMMON/QUEST/IQUEST(100)
      CHARACTER*8  DELTIM
      DIMENSION    IUHEAD(400)
      DIMENSION    IOCR(100)
      PARAMETER (JBIAS=2)
      PARAMETER (IEVENT=0)
 
      NREC = 0
      CALL FMRTIM(DELTIM)
      CALL TIMED(T)
   1  CONTINUE
      NHEAD = 400
      IXDIV = 0
      CALL FZIN(LUN,IXDIV,LSUP,JBIAS,'S',NHEAD,IUHEAD)
      IF(IQUEST(1).LT.4) THEN
         NREC = NREC + 1
         GOTO 1
      ENDIF
 
      PRINT *,'READFZ. end after ',NREC,' records, IQUEST(1) = ',
     +        IQUEST(1)
      CALL FMRTIM(DELTIM)
      CALL TIMED(T)
      PRINT *,'READFZ. Elapsed time = ',DELTIM,
     +        ' CP time = ',T,' sec.'
      END
