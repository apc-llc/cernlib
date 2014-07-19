*
* $Id: fatshow.F,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatshow.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
      PROGRAM FATUSER
*----------------------------------------------------------------------*
*                                                                      *
* Example FATUSER program. Just initialise ZEBRA, then FATMEN          *
* and call a couple of routines...                                     *
*                                                                      *
*----------------------------------------------------------------------*
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
      COMMON /QUEST/IQUEST(100)
      COMMON /FATUSE/ IDEBFA, IDIVFA, IKDRFA, KOFSFA, KOFUFA, LBFXFA
     +              , LSAVFA, LTOPFA, LBBKFA, LBGNFA, LTDSFA, LBDSFA
     +              , LPRTFA, NTOPFA, LUFZFA, IOUPFA, IOBKFA, IODSFA
     +              , LLNLFA, LLNHFA
*
      PARAMETER       (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
*
*
*     Initialise ZEBRA
*
      CALL MZEBRA(-3)
      CALL MZSTOR(IXSTOR,'/CRZT/','Q',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(LURCOR))
      CALL MZLOGL(IXSTOR,-3)
 
*
* *** Define user division and link area like:
*
      CALL MZDIV  (IXSTOR, IXDIV, 'USERS', 50000, LURCOR, 'L')
      CALL MZLINK (IXSTOR, '/USRLNK/', LUSRK1, LUSRLS, LUSRK1)
*
*     Unit for RZ database
*
      LUNRZ = 1
      LUNFZ = 2
 
*         read mode
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/CNDIV',IRC)
      CALL FMSHOW('//CERN/CNDIV/JAMIE/FATCAT',LFAT,KEYS,'A',IRC)
      END
 
