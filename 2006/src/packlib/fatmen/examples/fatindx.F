*
* $Id: fatindx.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fatindx.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
#include "fatmen/quest.inc"
#include "fatmen/fatpara.inc"
      CHARACTER*6  DENS
      CHARACTER*8  LIB
      CHARACTER*4  LABTYP
      CHARACTER*1  MNTTYP
      CHARACTER*8  MODEL
      CHARACTER*7  ROBMAN(2)
      DATA         ROBMAN(1)/'-Robot '/,ROBMAN(2)/'-Manual'/
      PARAMETER (LKEYFA=10)
      PARAMETER (MAXFIL=100)
      DIMENSION KEYS(LKEYFA,MAXFIL)
      CHARACTER*255 FILES(MAXFIL)
      DIMENSION JSORT(MAXFIL)
      CHARACTER*8   THRONG
      CHARACTER*255 TOPDIR
      CHARACTER*26  CHOPT
      CHARACTER*8   DSN
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
*     Units for FATMEN RZ/FZ files
*
      LUNRZ = 1
      LUNFZ = 2
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,'//CERN/CNDIV',IRC)
*
*     Get list of file names
*
      JCONT  = 0
      NTOTAL = 0
10    CONTINUE
      CALL FMLFIL('//CERN/CNDIV/JAMIE/DD/*',
     +FILES,KEYS,NFOUND,MAXFIL,JCONT,IRC)
      NTOTAL = NTOTAL + NFOUND
*
*     Now sort this list by VID & FSEQ
*
      CALL FMINDX(FILES,KEYS,NFOUND,JSORT,' ',IRC)
 
      DO 20 J=1,NFOUND
 
         LBANK = 0
         CALL FMSHOW(FILES(JSORT(J))(1:LENOCC(FILES(JSORT(J)))),
     +               LBANK,KEYS(1,JSORT(J)),'GM',IRC)
 
20    CONTINUE
 
      IF(IRC.NE.0) THEN
         JCONT = 1
         GOTO 10
      ENDIF
30    CONTINUE
      PRINT *,'Total of ',NTOTAL,' files found'
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
 
      END
 
