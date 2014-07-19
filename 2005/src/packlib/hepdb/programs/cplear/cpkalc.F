*
* $Id: cpkalc.F,v 1.1.1.1 1996/02/28 16:23:36 mclareni Exp $
*
* $Log: cpkalc.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
      SUBROUTINE CPKALC(CHDIR)
      CHARACTER*(*) CHDIR
      COMMON/USRLNK/IDIV,LADDR
      PARAMETER    (NKEYS=2)
      PARAMETER    (MAXOBJ=1000)
      DIMENSION     KEYS(13)
      DIMENSION     KEYZ(4)
      CHARACTER*8   CHTAG(NKEYS)
      CHARACTER*2   CHFOR
      CHARACTER*255 CHPATH,CHSAVE
#include "rzcl.inc"
#include "rzk.inc"
      DATA          NENTRY/0/
      SAVE          NENTRY
#include "q_jbit.inc"
* Ignoring t=pass
 
      IF(NENTRY.EQ.0) THEN
         NENTRY = 1
         RETURN
      ENDIF
*
*     Must save directory in local variable: calls to RZ
*     overwrite it!
*
      LDIR   = LENOCC(CHDIR)
      CHSAVE = CHDIR(1:LDIR)
*
*     Make directories in HEPDB database
*
      DELTA = 0.0
      IPREC = 0
      CHFOR = 'HH'
      CHTAG(1) = 'DETECTOR'
      CHTAG(2) = 'POINTER '
*
*     Construct directory name for HEPDB file
*
      LSLASH   = INDEXB(CHSAVE(1:LDIR),'/') + 1
      IF(INDEX(CHSAVE(1:LDIR),'MONTE').EQ.0) THEN
         CHPATH   = '//CDCD/CALIBRATION/'//CHSAVE(LSLASH:LDIR)
         LPATH    = LDIR - LSLASH + 20
      ELSE
         CHPATH   = '//CDCD/'//CHSAVE(LSLASH:LDIR)
         LPATH    = LDIR - LSLASH + 8
      ENDIF
      CALL CDMDIR(CHPATH(1:LPATH),NKEYS,CHFOR,CHTAG,MAXOBJ,
     +            IPREC,DELTA,'CP',IRC)
*
*     Loop over objects in current directory (file RZKAL.DATA)
*
      CALL RZCDIR(CHSAVE(1:LDIR),' ')
*
*     Retrieve the keys in this directory
*
      IF(LQRS.EQ.0)  GOTO 99
      IF(LCDIR.EQ.0) GOTO 99
      LS = IQ(KQSP+LCDIR+KLS)
      LK = IQ(KQSP+LCDIR+KLK)
      NK = IQ(KQSP+LCDIR+KNKEYS)
      NWK= IQ(KQSP+LCDIR+KNWKEY)
      DO 10 I=1,NK
 
         K=LK+(NWK+1)*(I-1)
         DO 20 J=1,NWK
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEYZ(J)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYZ(J),1)
            ENDIF
   20    CONTINUE
 
         CALL VZERO(KEYS,10)
         CALL UCOPY(KEYZ(1),KEYS(11),4)
*
*     Retrieve the highest cycle of this object
*     (will need modification if all cycles are to be converted)
*
         ICYCLE = 9999
         JBIAS = 2
         CALL RZIN(IDIV,LADDR,JBIAS,KEYZ,ICYCLE,' ')
         IF(IQUEST(1).NE.0) THEN
            PRINT *,'CPKALC. error ',IQUEST(1),' from RZIN for ',KEYZ
            GOTO 10
         ENDIF
*
*     Date/time of insertion
*
         CALL RZDATE(IQUEST(14),IDATE,ITIME,1)
         CALL CDPKTM(IDATE,ITIME,IPACK,IRC)
         KEYS(4) = IPACK
*
*     Store objects in HEPDB with appropriate keys
*     Option H: honour insertion time in KEYS(IDHINS)
*
         CALL CDSTOR(CHPATH(1:LPATH),LADDR,LKYBK,IDIV,KEYS,'H',IRC)
*
*     Reset directory
*
         CALL RZCDIR(CHSAVE(1:LDIR),' ')
*
*     Drop this bank
*
         CALL MZDROP(IDIV,LADDR,' ')
         LADDR = 0
 
   10 CONTINUE
 
   99 CONTINUE
 
      CALL RZCDIR(CHSAVE(1:LDIR),' ')
 
      END
