*
* $Id: fat2sql.F,v 1.1.1.1 1996/03/07 15:17:37 mclareni Exp $
*
* $Log: fat2sql.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:37  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PARAMETER (LURCOR=200000)
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
*
      COMMON /USRLNK/LUSRK1,LUSRBK,LUSRLS
*
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
      PARAMETER (LKEYFA=10)
      PARAMETER (MAXFIL=3000)
      PARAMETER (IMODE=0)
      DIMENSION KEYS(LKEYFA,MAXFIL)
      CHARACTER*255 FILES(MAXFIL)
      CHARACTER*80  CHSTEM
      CHARACTER*16  CHSYS,CHGRP
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
*     Get system and group name
*
      CALL FMQFAT(CHSYS,CHGRP,IRC)
      IF(IRC.NE.0) CALL FMSTOP
      CHSTEM  = CHSYS(1:LENOCC(CHSYS))//'/'//CHGRP(1:LENOCC(CHGRP))
      LCHSTEM = LENOCC(CHSTEM)
*
*     Units for FATMEN RZ/FZ files
*
      LUNRZ = 1
      LUNFZ = 2
*
*     Initialise FATMEN
*
      CALL FMINIT(IXSTOR,LUNRZ,LUNFZ,CHSTEM(1:LCHSTEM),IRC)
      CALL FMLOGL(1)
*
*     Connect to ORACLE
*
      CALL FMLOGI(IRC)
*
*
*     Get list of file names
*
      JCONT = 0
1     CONTINUE
      CALL FMLFIL(CHSTEM(1:LCHSTEM)//'/*/*',
     +FILES,KEYS,NFOUND,MAXFIL,JCONT,IRC)
      IF(IRC.EQ.-1) THEN
        JCONT = 1
      ELSE
        JCONT = 0
      ENDIF
 
      PRINT *,NFOUND,' files found'
 
      DO 10 I=1,NFOUND
      LENF = LENOCC(FILES(I))
      PRINT *,'Processing ',FILES(I)(1:LENF)
      LBANK = 0
      CALL FMGETK(FILES(I)(1:LENF),LBANK,KEYS(1,I),IRC)
      CALL FMZTOR(FILES(I)(1:LENF),LBANK+KOFUFA,IMODE,IRC)
      CALL MZDROP(IXSTOR,LBANK,' ')
10    CONTINUE
      IF(JCONT.NE.0) GOTO 1
*
*
*     Terminate cleanly
*
      CALL FMEND(IRC)
 
      END
 
