*
* $Id: cdexam1.F,v 1.1.1.1 1996/02/28 16:23:54 mclareni Exp $
*
* $Log: cdexam1.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:54  mclareni
* Hepdb, cdlib, etc
*
*
      PROGRAM CDEXA1
*     ==============
*
*     Create a new, empty database
*
      PARAMETER   (NWPAW=100000)
      COMMON/PAWC/PAW(NWPAW)
*
*     Initialise Zebra, HBOOK and HEPDB
*
      CALL CDPAW(NWPAW,NHBOOK,IDIV,'USR-DIV',5000,50000,'ZPHU',IRC)
*
*     Unit for database access
*
      LUNCD  = 1
*
*     Database parameters
*
      NPAIR  = 1
      NPRE   = 200
      NTOP   = 1
      NQUO   = 65000
*
*     Accept default record length (1024 words)
*
      LRECL  = 0
      CALL CDNEW(LUNCD,'HEPDB','HEPDB.DBS',IDIV,NPAIR,NQUO,NPRE,NTOP,
     +           LRECL,'F',IRC)
*
*     Set the log level
*
      CALL CDLOGL(' ',3,'A',IRC)
*
*     Terminate
*
      CALL CDEND(' ','A',IRC)
 
      END
