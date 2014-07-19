*
* $Id: dzdocm.F,v 1.1.1.1 1996/03/04 16:12:55 mclareni Exp $
*
* $Log: dzdocm.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:55  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDOCM(LUNINP,LUNPRO,CHOPT)
************************************************************************
*.                                                                     *
*...DZDOCM is the master routine of the DZDOC package                  *
*.                                                                     *
*.    SUBROUTINE DZDOCM(LUNPRO,CHOPT)                                  *
*.                                                                     *
*.  DZDOCM is the main user subroutine called from the main program    *
*.  It reads the input file on stream LUNIN, and decodes it            *
*.  according to given key words.                                      *
*.  Bank descriptor cards start with *B in columns 1 and 2.            *
*.  All other cards are ignored.                                       *
*.  The structural information associated with these various data      *
*.  items is stored in ZEBRA data structures for later retrieval.      *
*.                                                                     *
*. INPUT:                                                              *
*. ------
*.   LUNINP : Unit for bank descriptor cards                           *
*.   LUNPRO : Output unit for diagnostics                              *
*.            If LUNPRO= 0 then the default unit LUNOUT (6) is used    *
*.   CHOPT  : 'I' Only initialize the DZDOC system - Do not read cards *
*.            'R' Write each bank descriptor as a separate entry into  *
*.                the CWD of the declared RZ file with a key the bank  *
*.                name (RZ file must be write accessible i.e. the file *
*.                must have been declared using RZMAKE or RZFILE)      *
*.                                                                     *
*. CALLS    : DZDASM,DZDBKC,BNKSUM,CONSUM,DZDINI,LENNOC                *
*. CALLS    : INDEXC,REFSUM,REPSUM,USRSUM                              *
*. CALLED   : User called (DZMAIN)                                     *
*. COMMON   : DZDBSC,DZDBSN,DZDINC,DZDPAC,DZDPNC,DZDPRC,DZDRPC         *
*. COMMON   : DZDRPN,DZDTAP,DZDTIN,DZDTOK,DZDTYP,DZDUSC,DZDUSN,QUEST   *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 2.04(36)  /  1 Dec 1987                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
      CHARACTER*(*) CHOPT
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
#include "dzdoc/title.inc"
#include "dzdoc/linout.inc"
*    From DZEBRA
#include "zebra/zbcdk.inc"
 
*--     Define which options are desired
      CALL DZDSCO(CHOPT,IFLOPT)
 
*--     Initialize the system and calculate IO descriptors when needed
 
      LUNUSR = LUNPRO
      LUNIN = LUNINP
      CALL DZDINI
C       Only initialization desired
      IF (IFLOPT(MPOSIQ).NE.0)                             GO TO 999
      CALL MZIOCH(JIOPBK,1,'5I -H')
 
 1001 CONTINUE
 
*--     Read new card and take action depending on first characters
 
          CALL DZDCRD(LUNIN,CARD,IRC)
          IF(IRC.NE.0)GOTO 999
*          READ (LUNIN,'(A)',END=999) CARD
 
          IF (CARD(1:4).EQ.'*B..') THEN
*----          *B..  CARD. -- Beginning of bank descriptor
              CALL DZDBKC(*999)
          ENDIF
 
                                                           GO TO 1001
 
  999 END
