*
* $Id: fatpoke.F,v 1.1.1.1 1996/03/07 15:18:00 mclareni Exp $
*
* $Log: fatpoke.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:00  mclareni
* Fatmen
*
*
*
*     Example program to update the file creation time
*
*     The book-keeping file is included in DECK=BOOK,T=DATA.
*
      CHARACTER*133 CHLINE
      CHARACTER*255 GENAM,CHFILS
      CHARACTER*23  CHDATE
      CHARACTER*4   CHRUN
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      PARAMETER (NWDSFA=145)
      DIMENSION IVECT(NWDSFA)
      PARAMETER (MCRTFA=94)
      LUNRZ = 1
      LUNFZ = 2
*
*     Initialise FATMEN
*
      CALL FMSTRT(LUNRZ,LUNFZ,'//CERN/SMC',IRC)
*
*     Open book-keeping file
*
      OPEN(10,ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='OLD')
*
*     Process each record in the book-keeping file
*
10    CONTINUE
      READ(10,'(A)',END=99) CHLINE
 
      CHRUN  = CHLINE(8:11)
      CHDATE = CHLINE(13:35)
      CALL FMPKVX(CHDATE,ID,IT,IP,IRC)
*
*     Find entries corresponding to this run
*
      GENAM = '//CERN/SMC/P2D92/*/R' // CHRUN
*
*     Assume that there will only be one match
*
      MAXFIL = 1
      JCONT  = 0
 
      CALL FMLFIL(GENAM,CHFILS,KEYS,NFOUND,MAXFIL,JCONT,IRC)
*
*     Get the entry for this file
*
      CALL FMPEEK(CHFILS,IVECT,'A',IRC)
*
*     Display the entry
*
      CALL FMSHOW(CHFILS,-1,KEYS,'GT',IRC)
*
*     Update the creation time
*
      CALL FMPUTI(-1,IP,MCRTFA,IRC)
*
*     Display the entry
*
      CALL FMSHOW(CHFILS,-1,KEYS,'GT',IRC)
*
*     and stuff it back
*
*-----CALL FMPOKE(CHFILS,IVECT,'NR',IRC)
 
      GOTO 10
 
99    CONTINUE
 
      CLOSE(10)
 
      END
