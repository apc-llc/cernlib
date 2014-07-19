*
* $Id: bkwrp.inc,v 1.1.1.1 1996/03/04 16:12:59 mclareni Exp $
*
* $Log: bkwrp.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:59  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_BKWRP_INC
#define CERNLIB_DZDOC_BKWRP_INC
*
*
* bkwrp.inc
*
      PARAMETER (LNCARD=80 , LNCAR4=LNCARD/4)
      CHARACTER CARD*(LNCARD)
      COMMON /DZDINC/ CARD
 
      PARAMETER (IRLVMQ = 10)
 
C--     DESCRIPTION OF COMMON DZDTYP
C-      ============================
 
C--   Common DZDTYP contains the decoded information corresponding to
C--   one "CARD" image in variable CARD. Is contains also the back-up
C--   i.e. the old valus of the status flags to treat continuation info
 
C--     VARIABLE                        DESCRIPTION
C-      ========                        ===========
C       ISTATE        Code of state for last valid card ID
C       IDTAG         Code of character identification of card
C       ICONT         Card is continuation of previous one
C       ISLASH        Flag set when last character of CARD is '/'
C                     This character is used to generate a line feed
C                     for multi-line entries
C       IISLSH        Value of ISLASH for previous CARD
C       IREPLV        Repetition level
C       IREPLO        Old repetition level
C       ISCHLO        Old CHOICE section flag
C       IDEMTX        Hollerith code for text as previous entry (DZDINI)
C       IILFLF        Hollerith code for line feed (DZDINI)
C       INUM1         First number if number interpretation possible
C       IHNUM1        Vector containing the Hollerith representation
C                     of the first  number field (if non-numeric)
C                     INUM1 then contains -16* number of words used
C       INUM2         Second number if number interpretation possible
C       IHNUM2        Vector containing the Hollerith representation
C                     of the second number field (if non-numeric)
C                     INUM2 then contains -16* number of words used
C       NBNKS         Number of bank descriptors present
C       NHTAG         Number of characters in the tag field of the card
C                     This field can also correspond to a declared
C                     numeric field with a non-numeric entry
C       NWTAG         Number of 4-byte machine words needed for the tag
C       IHTAG         Vector containing the Hollerith representation
C                     of the tag field of the card
C       NHTXT         Number of characters in the text field of the card
C       NWTAG         Number of 4-byte machine words needed for the text
C       IHTAG         Vector containing the Hollerith representation
C                     of the text field of the card
 
      COMMON/DZDTYP/ISTATE,IDTAG,ICONT,ISLASH,IISLSH,IREPLV,IREPLO,
     X       IDEMTX,IILFLF,INUM1,IHNUM1(LNCAR4),INUM2,IHNUM2(LNCAR4),
     X       NBNKS,NHTAG,NWTAG,IHTAG(LNCAR4),NHTXT,NWTXT,IHTXT(LNCAR4)
 
#include "dzdoc/dzdocc.inc"
 
#include "zebra/mqsys.inc"
#include "zebra/qequ.inc"
#include "zebra/mzcn.inc"

#endif
