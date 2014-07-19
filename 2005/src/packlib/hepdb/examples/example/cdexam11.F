*
* $Id: cdexam11.F,v 1.1.1.1 1996/02/28 16:23:55 mclareni Exp $
*
* $Log: cdexam11.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:55  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA11
*     ===============
*
*     Test Fortran CDUSE and CDVECT routines
*
      COMMON/QUEST/IQUEST(100)
      PARAMETER     (NWPAW=100000)
      COMMON/PAWC/NWP,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(NWPAW)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      common/mylink/lbank
      PARAMETER     (NKEYS=14)
      PARAMETER     (MAXOBJ=1000)
      PARAMETER     (NW=100)
      DIMENSION     KEYS(NKEYS),MASK(NKEYS)
      dimension     numrun(2)
      dimension     ivect(100)
      CHARACTER*10  CHFOR
      CHARACTER*4   CHTOP
      CHARACTER*80  CHFILE
      CHARACTER*8   CHTAG(NW)
 
      NWP = NWPAW
*
*     Initialise Zebra, HBOOK and HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,NWPAW,'ZPHU',IRC)
*     call hlimit(nwpaw)
      call mzlogl(ixpawc,3)
      call mzlink(ixpawc,'/MYLINK/',lbank,lbank,lbank)
*
*     Unit for database access
*
      LUNCD  = 1
*
*     Unit for database update (via journal files)
*
      LUNFZ  = 2
*
*     Find the database file and construct the top directory name
*
      CALL CDPREF(10,'W8',CHTOP,CHFILE,IRC)
*
*     Open the database file
*
      LRECL  = 0
      CALL CDOPEN(LUNCD,LUNFZ,CHTOP,CHFILE,LRECL,IHDIV,' ',IRC)
*
      CALL CDLOGL(' ',3,'A',IRC)
*
*     Get objects using full path
*
      NUMRUN(1) = 9
      numrun(2) = 3
      CALL CDUSE('//CDW8/TRG',LBANK,NUMRUN,' ',IRC)
      print 9001,iquest(1),iquest(2)
9001  format(' IQUEST(1-2) = ',I10,1X,I10)
*
*     Get vector
*
      lvect = 100
      CALL CDVECT('//CDWG/TRG',IVECT,LVECT,LBANK,'G',IRC)
      print *,lvect,irc
      print *,(ivect(i),i=1,10)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
      CALL MZEND
 
      END
