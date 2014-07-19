*
* $Id: dzdcar.F,v 1.1.1.1 1996/03/04 16:12:54 mclareni Exp $
*
* $Log: dzdcar.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:54  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCAR(ICARD,*)
************************************************************************
*.                                                                     *
*...DZDCAR interprets the tag fields of the bank descriptor cards      *
*.                                                                     *
*. Characters 4 and 5 of the input line are tested against all possible*
*. tags. If a valid tag is found, consistency checks are made.         *
*. If no valid tag line interpretation is possible, the line is treated*
*. as a continuation line of the previous tag with pure text.          *
*.                                                                     *
*. INPUT:                                                              *
*. ------                                                              *
*.   ICARD  : Integer containing sequence number of card being read    *
*.                                                                     *
*. CALLS    : DZD0NT,DZD0TT,DZD00T,DZDNUM,UCTOH,VBLANK                 *
*. CALLED   : DZDBKC                                                   *
*. COMMON   : DZDINC,DZDTAP,DZDTYP                                     *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 2.01(22)  / 27 Aug 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
      CHARACTER CTAG*2
#include "dzdoc/bktgdatq.inc"
 1001 CONTINUE
      ISLASH = 0
      CALL VBLANK(IHTAG,LNCAR4)
      CALL VBLANK(IHTXT,LNCAR4)
 
      IF (CARD(4:4).EQ.' ') THEN
*--         Continuation field
 
*--        Get first and last non-blank characters and fill IHTXT vector
 
          ITXBEG = INDEXC(CARD(5:),' ') + 4
          IF (ITXBEG.EQ.4) THEN
C-              Empty text field
              NWTXT  = 0
              NWTXT  = 0
          ELSE
*--             If user defined line-feed respect his layout
*Otto 21-jul-89
**
*              IF (IISLSH.NE.0) THEN
*                  ITXBEG = 5
*              ELSE
*--                 Force at least one blank before first word
                  ITXBEG = ITXBEG - 1
*              ENDIF
              ITXEND = INDXBC(CARD,' ')
              NHTXT  = ITXEND - ITXBEG + 1
              NWTXT  = (NHTXT+3)/4
              CALL UCTOH(CARD(ITXBEG:),IHTXT,4,NHTXT)
              IF (CARD(ITXEND:ITXEND).EQ.'/') THEN
                  ISLASH = 1
              ELSE
                  ISLASH = 0
              ENDIF
          ENDIF
          NWTAG  = 0
          NWTAG  = 0
          ICONT  = 1
                                                           GO TO 999
      ENDIF
 
*--     Loop over possible tags to choose the one needed
 
      ICONT  = 0
      CTAG   = CARD(4:5)
      DO 100 I=1,ITGIDQ
          IF (CTAG.EQ.CBKTAC(I))  THEN
*--             Get state
              IF (IBKTAC(MBKASQ,I).EQ.ISREPQ) THEN
C-                  Increment rep level
                  IF (IREPLV.EQ.IRLVMQ) THEN
                      WRITE(LUNOUT,'(''0'',I6,3X,A,/,
     X                      '' ??Too many (>10) repetition levels '',
     X                      ''-- scan abandoned'')') ICARD,CARD
                                                           GO TO 998
                  ENDIF
                  IREPLV = IREPLV + 1
              ELSEIF (IBKTAC(MBKASQ,I).EQ.ISCHOQ) THEN
C-                  Flag start of choice section
                  ISCHLO = 0
              ELSE
C-                  Reinitialize ISTATE
                  ISTATE = IBKTAC(MBKASQ,I)
              ENDIF
              IDTAG  = I
              IDECOD = IBKTAC(MBKAGQ,I)
              IF (IDECOD.EQ.IBKANQ) THEN
C-                  Text only
                  CALL DZD00T
              ELSEIF(IDECOD.EQ.IBKAMQ) THEN
C-                  Number field
                  CALL DZD0NT
              ELSEIF(IDECOD.EQ.IBKASQ) THEN
C-                  Special decoding required
                  CONTINUE
              ELSE
C-                  Standard tag field
                  CALL DZD0TT
              ENDIF
                                                           GO TO 999
          ENDIF
  100 CONTINUE
 
*--     Fallen through DO loop --> it should be a description of entries
 
      IF (ISTATE.GT.ISGENQ) THEN
          CALL DZDNUM(*998)
          IDTAG  = ITGENQ
C-          Standard tag field
          CALL DZD0TT
                                                           GO TO 999
      ELSE
          WRITE(LUNOUT,'(''0'',I6,3X,A,/,
     X          '' ?? Invalid tag '',A,
     X          '' -- scan abandoned'')') ICARD,CARD,CTAG
                                                           GO TO 998
      ENDIF
 
  998 RETURN 1
  999 END
