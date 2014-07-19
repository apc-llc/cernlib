*
* $Id: dzdbkc.F,v 1.1.1.1 1996/03/04 16:12:53 mclareni Exp $
*
* $Log: dzdbkc.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:53  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBKC(*)
************************************************************************
*.                                                                     *
*...DZDBKC master routine for decoding bank descriptor cards           *
*.                                                                     *
*.                                                                     *
*. BANKS  L : QBKD                                                     *
*. BANKS  M : QBKD,QBKM                                                *
*. CALLS    : DZDCAR,DZDEOS,IFROMC,INDXBC,MZCHLS,MZIOBK,MZLIFT,MZPUSH  *
*. CALLS    : UCTOH,ZBINSR                                             *
*. CALLED   : DZDOCM                                                   *
*. COMMON   : DZDINC,DZDOCC,DZDSOC,DZDTAP,DZDTYP,MZCN,MZDROP,RZOUT     *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US
*.                                                                     *
*. VERSION  : 2.03(36)  / 27 Sep 1987                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
C-      Min. nb. of words in bank for push and nb. of words pushed
      PARAMETER (MBPSHQ=30,NBPSHQ=200)
#include "dzdoc/docparq.inc"
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
#include "dzdoc/title.inc"
 
      CHARACTER CIDBK*4
      INTEGER MBKBKD(6),IUIDEM
 
*------OTTO-  KEY vector for RZIN/RZOUT
 
      INTEGER KEYVEC(2)
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/linout.inc"
*    From DZEBRA
#include "zebra/zbcdk.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"
      DATA MBKBKD/0,0,0,500,0,0/ , IFIRST/0/
 
 1001 CONTINUE
      IF (IFIRST.EQ.0) THEN
*--        Initialization of the name constants
*--         Get the bank NAME vector for the documenation banks
*--         complete by describing its structure as counters at the
*--         bank beginning and then self-describing sectors.
          CALL UCTOH ('QBKD',MBKBKD,4,4)
          CALL MZIOBK(MBKBKD,5,'1H 19I -S')
          IFIRST=1
      ENDIF
 
   10 NBNKS  = NBNKS +1
      CIDBK  = CARD(5:8)
*     no blanks allowed on IDBK
      IPBL=INDEX(CIDBK,' ')
      IF(IPBL.GT.0 .AND. IPBL.LT.4)CIDBK(IPBL+1:)=' '
      IUIDEM=0
      CALL UCTOH(CIDBK,IDBK,4,4)
*      WRITE(LUNOUT,'(A,A)')' Formatting ',CIDBK
 
*---OTTO--  set KEY(2) = blank, force no formatting
      LFORCE=1
      CALL UCTOH('NONE',IHIDUP,4,4)
 
      LIDBK  = LZFIND(0,LDQBKD,IDBK,MBIDHQ)
 
*    allow same bank Id more than once
 
*      IF (LIDBK.EQ.0) THEN
 
*--         Create a new text buffer bank
          MBKBKD(1)=IDBK
          CALL MZLIFT(0,LQBKD,LDQBKD,1,MBKBKD,0)
*--         Number of words in use
          IQ(KQSP+LQBKD+MBUSEQ) = NBHEAQ
*--         Number of words in header
          IQ(KQSP+LQBKD+MBHEAQ) = NBHEAQ
*--         Introduce encoded name
          IQ(KQSP+LQBKD+MBIDHQ) = IDBK
*--         Introduce the creation sequence number as IDN
          IQWIDN(KQSP+LQBKD)    = NBNKS
 
*      ELSE
*--         Text bank for given bank identifier already exists
*          WRITE(LUNOUT,'(''0DZDOC -- Changing existing descriptor '',
*     X          ''presently not supported'')')
*                                                              GO TO 998
*      ENDIF
      ISTOLD = ISGENQ
      ISTATE = ISGENQ
*--     Get hold of text accompanying bank identifier
      ISLASH = 0
      CALL DZD00T
*--     Hollerith word count
      INSERT = NBHEAQ + 1
*--     Insert the field indicating id = 0 and tag = 1 word
      IQ(KQSP+LQBKD+INSERT) = IFOINQ+JFOSEQ*(NBDSCQ-1)
      IQ(KQSP+LQBKD+INSERT+MBPATQ) = 1
      INSERT = INSERT + NBDSCQ
      IICONT = INSERT
*--     Then introduce the bank Hollerith id. and the text
      IQ(KQSP+LQBKD+INSERT) = IFOHOQ+JFOSEQ*(NWTXT+1)
      INSERT = INSERT + 1
      CALL UCTOH(CIDBK,IQ(KQSP+LQBKD+INSERT),4,4)
      INSERT = INSERT + 1
      IF (NWTXT.GT.0) CALL UCOPY(IHTXT,IQ(KQSP+LQBKD+INSERT),NWTXT)
      IISLSH = ISLASH
      ITGLST = NWTXT + 2
*--     Update the number of words in use
      IQ(KQSP+LQBKD+MBUSEQ) = IQ(KQSP+LQBKD+MBUSEQ) + ITGLST + NBDSCQ
      IQ(KQSP+LQBKD+MBGENQ) = IQ(KQSP+LQBKD+MBGENQ) + ITGLST + NBDSCQ
 
      DO 500 ICARD = 1,99999
C
*--         Read the bank description cards one by one
C
          CALL DZDCRD(LUNIN,CARD,IRC)
          IF(IRC.NE.0)GOTO 20
*          READ (LUNIN,'(A)',END=20) CARD
          IRETUR=0
          GOTO 21
20        CONTINUE
*         force output of doc at EOF
          IRETUR=1
          CARD(1:2)=' '
21        CONTINUE
          IF (CARD(1:2).NE.'*B' .OR. CARD(3:4).EQ.'..') THEN
*             end of doc for one bank, output doc
              IF(IUIDEM.NE.0)THEN
*                idem tag given, get bank and replace Up-bank Id
                 KEYVEC(1) = IDBK
                 KEYVEC(2)=IUIDEM
                 IF (IFLOPT(MPOSRQ).NE.0) THEN
*                   its in a RZ-file
                    IF(LDQBKD.NE.0)CALL MZDROP(0,LDQBKD,'.')
                    LDQBKD=0
                    ICYCLE = 1000000
                    CALL RZIN(0,LSAME,2,KEYVEC,ICYCLE,' ')
                    IF(LSAME.EQ.0 .OR. IQUEST(1) .NE. 0)THEN
                       WRITE(LUNOUT,'(A,2A4)')
     &                 ' Cant find Idem for: ',KEYVEC
                       GOTO 998
                    ENDIF
                    GOTO 29
                 ENDIF
*                its in a linear chain of banks
                 LSAME=LDQBKD
25               CONTINUE
                 IF (IQ(KQSP+LSAME-4).NE.IDBK
     &          .OR. IQ(KQSP+LSAME-5).NE.IUIDEM)THEN
                    LSAME=LQ(KQSP+LSAME)
                    IF(LSAME.NE.0)THEN
                       GOTO 25
                    ELSE
                       GOTO 50
                    ENDIF
                 ENDIF
*                bank found , delete the template, copy IDEM
                 CALL MZDROP(0,LQBKD,'.')
                 LQBKD=0
                 CALL MZCOPY(0,LSAME,2,LDQBKD,1,' ')
                 LSAME=LDQBKD
 
29               CONTINUE
*                find Up-bank info
*                 DO 50  IGEN = 1,NNGENQ
                    II     = NBHEAQ + 1
   30               IF (II-NBHEAQ.GT.IQ(KQSP+LSAME+MBGENQ))   GO TO 50
*--              Integer self-describing sector?
                    IF (MOD(IQ(KQSP+LSAME+II),JFOSEQ).NE.IFOINQ)THEN
*--                    Not an integer self-describing sector --> skip
                       II     = II + IQ(KQSP+LSAME+II)/JFOSEQ + 1
                       IF (II-NBHEAQ.GT.IQ(KQSP+LSAME+MBGENQ)) THEN
                          GO TO 50
                       ELSE
                          GO TO 30
                       ENDIF
                    ENDIF
*--                 Integer self-describing sector: look at info id.
                    INTAG  = IBITS(IQ(KQSP+LSAME+II+1),ICHIDQ,NCHIDQ)
 
                    IF (INTAG.NE.ITGUPQ) THEN
*--                    Not yet the id. needed --> continue
                       II     = II + IQ(KQSP+LSAME+II)/JFOSEQ + 1
                       IF (II-NBHEAQ.GT.IQ(KQSP+LSAME+MBGENQ)) THEN
                          GO TO 50
                       ELSE
                          GO TO 30
                       ENDIF
                    ENDIF
*--                 Upbank tag found, skip to text
                    II     = II + IQ(KQSP+LSAME+II)/JFOSEQ + 1
                    IF (MOD(IQ(KQSP+LSAME+II),JFOSEQ).NE.IFOHOQ)THEN
*                      not Holl section, fatal
                       WRITE(LUNOUT,*)' No Bank Id found'
                       GOTO 998
                    ENDIF
*                   replace Up-bank
                    IQ(KQSP+LSAME+II+1)=IHIDUP
                    LQBKD=LSAME
                    GOTO 51
50               CONTINUE
                 WRITE(LUNOUT,'(A,2A4)')
     &           ' Cant find Idem for: ',KEYVEC
                 GOTO 998
51               CONTINUE
              ENDIF
*
C--             Trim the bank to its minimal size
              CALL MZPUSH(0,LQBKD,0,
     X                 IQ(KQSP+LQBKD+MBUSEQ)-IQWND(KQSP+LQBKD),'I')
              IQ(KQSP+LQBKD-5)=IHIDUP
              IF (IFLOPT(MPOSRQ).NE.0) THEN
*----------         Write the bank description to the RZ file
 
*----OTTO---
                  KEYVEC(1) = IDBK
                  KEYVEC(2) = IHIDUP
*                  CALL RZLDIR(' ',' ')
                  CALL RZOUT(0,LQBKD,KEYVEC,ICY,'.')
                  IF (IQUEST(1).NE.0) THEN
                      WRITE(LUNOUT,'(''0Problem Writing Bank '',
     X                   ''Descriptor '',A,'' to RZ file'')') CIDBK
                                                           GO TO 998
                  ENDIF
                  CALL MZDROP(0,LQBKD,'.')
                  LQBKD=0
              ENDIF
              IF(CARD(1:2).NE.'*B')THEN
                 GO TO 999
              ELSE
                 GO TO 10
              ENDIF
          ELSEIF(CARD(3:3).EQ.'.') THEN
*----------     It is a command or continuation --> decode according
*----------     characters 4 and 5
              CALL DZDCAR(ICARD,*998)
 
*---OTTO-- look if its up-bank, set KEY(2) = up-bank
 
              IF(IDTAG .EQ. ITGUPQ)IHIDUP = IHTAG(1)
              IF(IDTAG .EQ. ITGIDQ)THEN
                 IUIDEM = IHTAG(1)
              ENDIF
 
*--             Still enough room in bank to include information ?
              IF(IQWND(KQSP+LQBKD)-IQ(KQSP+LQBKD+MBUSEQ).LT.MBPSHQ) THEN
                  CALL MZPUSH(0,LQBKD,0,NBPSHQ,'I')
              ENDIF
C--------       First descriptor card
              IF (ICONT.EQ.0) THEN
*------             New card tag. Which is the new state identifier?
                  IF (ISTATE.EQ.ISGENQ) THEN
*----                   General information: enter in front part of bank
                      II     = NBHEAQ + 1
                      DO 200 I=1,99999
                          IF(II-NBHEAQ.GT.IQ(KQSP+LQBKD+MBGENQ)) THEN
                                                           GO TO 210
                          ENDIF
*--                         Integer self-describing sector?
                          IF (MOD(IQ(KQSP+LQBKD+II),JFOSEQ).NE.IFOINQ)
     X                                                              THEN
*--                      Not an integer self-describing sector --> skip
                              II     = II + IQ(KQSP+LQBKD+II)/JFOSEQ + 1
                              IF(II-NBHEAQ.GT.IQ(KQSP+LQBKD+MBGENQ))THEN
                                                           GO TO 210
                              ELSE
                                                           GO TO 200
                              ENDIF
                          ENDIF
*--                     Integer self-describing sector: look at card id.
                          INTAG=IBITS(IQ(KQSP+LQBKD+II+1),ICHIDQ,NCHIDQ)
                          IF (INTAG.NE.IDTAG) THEN
*--                         Not yet the id. needed --> continue
                              II     = II + IQ(KQSP+LQBKD+II)/JFOSEQ + 1
                              IF(II-NBHEAQ.GT.IQ(KQSP+LQBKD+MBGENQ))THEN
                                                           GO TO 210
                              ELSE
                                                           GO TO 200
                              ENDIF
                          ELSE
*--                             Duplicated tag
                              WRITE(LUNOUT,
     X                        '(''0'',I6,3X,A,/,'' ?? Duplicated tag '',
     X                         A,'' Scan abandoned'')')
     X                         ICARD,CARD,CBKTAC(IDTAG)
                                                           GO TO 998
                          ENDIF
  200                 CONTINUE
*--                         Introduce new information
  210                 INSERT = II
*--                     Memorize tag sector for multiple choice bit
                      ISTEER = II
*--                     Get the information to copy
                      IQ(KQSP+LQBKD+INSERT+MBPATQ) = 0
                      ITGTYP = IBKTAC(MBKAGQ,IDTAG)
                      IF (ITGTYP.EQ.IBKAMQ) THEN
*--                         Number possible
                          IF (INUM1.NE.INUTGQ) THEN
*--                             Fill the descriptor fields
                              IQ(KQSP+LQBKD+INSERT+MBIX1Q) = INUM1
                          ELSE
                              IQ(KQSP+LQBKD+INSERT+MBIX1Q) = NWTAG
                              IQ(KQSP+LQBKD+INSERT+MBPATQ) = 3
                          ENDIF
                      ELSEIF (ITGTYP.EQ.IBKANQ) THEN
*--                         Only text
                          CONTINUE
                      ELSE
*--                         Indicate length of tag
                          IQ(KQSP+LQBKD+INSERT+MBPATQ) = ITGTYP
                      ENDIF
 
*--                     Set the identifier flag
                      CALL CBYT(IDTAG,1,
     X                IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHIDQ+1,NCHIDQ)
*                      CALL MVBITS(IDTAG,0,NCHIDQ,
*     X                            IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHIDQ)
*--                     Get the self-describing sector right
                      IQ(KQSP+LQBKD+INSERT) = IFOINQ + JFOSEQ*(NBDSCQ-1)
*--                     Memorize the bitted pattern word
                      IPAT   = IQ(KQSP+LQBKD+INSERT+MBPATQ)
                      INSERT = INSERT + NBDSCQ
*--                     Memorize the entry's address (for continuation)
                      IICONT = INSERT
*--                     Flag to signal presence Hollerith data last tag
                      ITGLST = 0
*--                     When there is tag info
                      IF (IBITS(IPAT,ICHTGQ,NCHTGQ).NE.0) THEN
*--                         Nb. of words for tag field
                          IF (ITGTYP.EQ.IBKAMQ) THEN
                              NTAG   = NWTAG
                          ELSE
                              NTAG   = ITGTYP
                          ENDIF
*--                         Hollerith word count
                          IQ(KQSP+LQBKD+INSERT) = IFOHOQ+JFOSEQ*NTAG
                          CALL UCOPY(IHTAG,IQ(KQSP+LQBKD+INSERT+1),NTAG)
                          ITGLST = NTAG + 1
                          INSERT = INSERT + ITGLST
                      ENDIF
                      IF (NWTXT.GT.0) THEN
*--                         Hollerith word count
                          IF (ITGLST.EQ.0) THEN
                              IQ(KQSP+LQBKD+INSERT) =IFOHOQ+JFOSEQ*NWTXT
                              ITGLST = 1
                              INSERT = INSERT + 1
                          ELSE
                              IQ(KQSP+LQBKD+IICONT) =
     X                        IQ(KQSP+LQBKD+IICONT) + JFOSEQ*NWTXT
                          ENDIF
                          CALL UCOPY(IHTXT,IQ(KQSP+LQBKD+INSERT),NWTXT)
                          ITGLST = ITGLST + NWTXT
                          INSERT = INSERT + NWTXT
                      ENDIF
*--                     Update the number of words in use
                      IQ(KQSP+LQBKD+MBUSEQ) = IQ(KQSP+LQBKD+MBUSEQ) +
     X                                      INSERT - II
                      IQ(KQSP+LQBKD+MBGENQ) = IQ(KQSP+LQBKD+MBGENQ) +
     X                                      INSERT - II
                  ELSE
*------                 Link, status-bits or data description
                      IF (ISTOLD.NE.ISTATE) THEN
*--                         Start of new description
  250                    IF (ISTATE.EQ.ISLINQ) THEN
                            IF (IQ(KQSP+LQBKD+MBLINQ).NE.0) THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? Duplicated '',
     X                           ''link descriptor'',
     X                           '' -- scan abandoned'')') ICARD,CARD
                                                           GO TO 998
                            ELSEIF ((IQ(KQSP+LQBKD+MBRLIQ).NE.0).OR.
     X                              (IQ(KQSP+LQBKD+MBBITQ).NE.0).OR.
     X                              (IQ(KQSP+LQBKD+MBDATQ).NE.0)) THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? LINK '',
     X                           ''descriptor out of order'',
     X                           '' -- scan abandoned'')') ICARD,CARD
                                                           GO TO 998
                            ENDIF
                            INSERT = NBHEAQ + IQ(KQSP+LQBKD+MBGENQ) + 1
                         ELSEIF (ISTATE.EQ.ISRLIQ) THEN
                            IF (IQ(KQSP+LQBKD+MBRLIQ).NE.0) THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? Duplicated '',
     X                           ''ref link descriptor -- '',
     X                           ''scan abandoned'')') ICARD,CARD
                                                           GO TO 998
                            ELSEIF ((IQ(KQSP+LQBKD+MBDATQ).NE.0) .OR.
     X                              (IQ(KQSP+LQBKD+MBBITQ).NE.0))THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? RefLink desc'',
     X                           '' out of order - scan abandoned'')')
     X                           ICARD,CARD
                                                           GO TO 998
                            ENDIF
                            INSERT = NBHEAQ + IQ(KQSP+LQBKD+MBGENQ) +
     X                                        IQ(KQSP+LQBKD+MBLINQ) + 1
                         ELSEIF (ISTATE.EQ.ISBITQ) THEN
                            IF (IQ(KQSP+LQBKD+MBBITQ).NE.0) THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? Duplicated '',
     X                           ''status  bit descriptor -- '',
     X                           ''scan abandoned'')') ICARD,CARD
                                                           GO TO 998
                            ELSEIF (IQ(KQSP+LQBKD+MBDATQ).NE.0) THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? BIT descriptor'',
     X                           '' out of order - scan abandoned'')')
     X                           ICARD,CARD
                                                           GO TO 998
                            ENDIF
                            INSERT = NBHEAQ + IQ(KQSP+LQBKD+MBGENQ) +
     X                                        IQ(KQSP+LQBKD+MBRLIQ) +
     X                                        IQ(KQSP+LQBKD+MBLINQ) + 1
                         ELSEIF (ISTATE.EQ.ISDATQ) THEN
                            IF (IQ(KQSP+LQBKD+MBDATQ).NE.0) THEN
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? Duplicated '',
     X                           ''data part descriptor -- '',
     X                           ''scan abandoned'')') ICARD,CARD
                                                           GO TO 998
                            ENDIF
                            INSERT = NBHEAQ + IQ(KQSP+LQBKD+MBGENQ) +
     X                                        IQ(KQSP+LQBKD+MBLINQ) +
     X                                        IQ(KQSP+LQBKD+MBRLIQ) +
     X                                        IQ(KQSP+LQBKD+MBBITQ) + 1
                         ELSE
                               WRITE(LUNOUT,
     X                         '(''0'',I6,3X,A,/,'' ?? Invalid state '',
     X                           I3)') ISTATE
                                                           GO TO 500
                         ENDIF
*--                        Memorize and zero state variables
                         IREPLO = 0
                         IREPLV = 0
                         ISTOLD = ISTATE
                                                           GO TO 500
                      ENDIF
*--                     Get the information to copy
                      II     = INSERT
*--                     For repetition series
                      IF (IREPLV.GT.IREPLO) THEN
*--                         Repetition field descriptor
                          IQ(KQSP+LQBKD+INSERT+MBPATQ) = 0
*--                         Fill the descriptor fields
                          IF (INUM1.NE.INUTGQ) THEN
*--                             Actual number
                              IQ(KQSP+LQBKD+INSERT+MBIX1Q) = INUM1
                          ELSE
*--                             Text field
                              IQ(KQSP+LQBKD+INSERT+MBIX1Q)=-JFOSEQ*NWTAG
                          ENDIF
*--                          Set the identifier flag
                          CALL CBYT(IDTAG,1,
     &                    IQ(KQSP+LQBKD+INSERT+MBPATQ), ICHIDQ+1,NCHIDQ)
*--                           and introduce the repetition level
                          CALL CBYT(IREPLV,1,
     X                    IQ(KQSP+LQBKD+INSERT+MBPATQ),IRPLVQ+1,NRPLVQ)
*--                         Get the self-describing sector right
                          IQ(KQSP+LQBKD+INSERT)=IFOINQ+JFOSEQ*(NBDSCQ-1)
                          INSERT = INSERT + NBDSCQ
                          IICONT = INSERT
                          IF (INUM1.EQ.INUTGQ) THEN
*--                           Hollerith word count
                            IQ(KQSP+LQBKD+INSERT) = IFOHOQ+JFOSEQ*NWTAG
                            INSERT = INSERT + 1
                            CALL UCOPY
     X                           (IHTAG,IQ(KQSP+LQBKD+INSERT),NWTAG)
                            INSERT = INSERT + NWTAG
                            ITGLST = NWTAG + 1
                          ELSE
                            ITGLST = 0
                          ENDIF
                          IF (NWTXT.GT.0) THEN
*--                           Hollerith word count
                            IF (ITGLST.EQ.0) THEN
                              IQ(KQSP+LQBKD+INSERT) =IFOHOQ+JFOSEQ*NWTXT
                              ITGLST = 1
                              INSERT = INSERT + 1
                            ELSE
                              IQ(KQSP+LQBKD+IICONT) =
     X                        IQ(KQSP+LQBKD+IICONT) + JFOSEQ*NWTXT
                            ENDIF
                            CALL UCOPY
     X                           (IHTXT,IQ(KQSP+LQBKD+INSERT),NWTXT)
                            ITGLST = ITGLST + NWTXT
                            INSERT = INSERT + NWTXT
                          ENDIF
                          IREPLO = IREPLV
                      ELSE
*--                         Memorize tag sector for multiple choice bit
                          ISTEER = INSERT
*--                         Fill out the tag fields
                          IQ(KQSP+LQBKD+INSERT+MBPATQ) = NWTENQ
                          IQ(KQSP+LQBKD+INSERT+MBIX1Q) = INUM1
                          IQ(KQSP+LQBKD+INSERT+MBIX2Q) = INUM2
*--                         Introduce the tag identifier
                          CALL CBYT(IDTAG,1,
     X                    IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHIDQ+1,NCHIDQ)
 
*--                           and introduce the repetition level
                          CALL CBYT(IREPLV,1,
     X                    IQ(KQSP+LQBKD+INSERT+MBPATQ),IRPLVQ+1,NRPLVQ)
*--                         Get the self-describing sector right
                          IQ(KQSP+LQBKD+INSERT) = IFOINQ +
     X                                          JFOSEQ*(NBDSCQ-1)
                          INSERT = INSERT + NBDSCQ
*--                      Memorize the entry's address (for continuation)
                          IICONT = INSERT
*--                         Nb. of words for tag field
*--                         Hollerith word count
                          IQ(KQSP+LQBKD+INSERT) = IFOHOQ+JFOSEQ*NWTENQ
                         CALL UCOPY
     X                        (IHTAG,IQ(KQSP+LQBKD+INSERT+1),NWTENQ)
                          ITGLST = NWTENQ + 1
                          INSERT = INSERT + ITGLST
*--                         Entries with non-numeric fields
                          IF (INUM1.LE.-JFOSEQ) THEN
                             NWN1   = -INUM1/JFOSEQ
                             IQ(KQSP+LQBKD+IICONT) =
     X                       IQ(KQSP+LQBKD+IICONT) - INUM1
                            CALL UCOPY
     X                           (IHNUM1,IQ(KQSP+LQBKD+INSERT),NWN1)
                             ITGLST = ITGLST + NWN1
                             INSERT = INSERT + NWN1
                          ENDIF
                          IF (INUM2.LE.-JFOSEQ) THEN
                             NWN2   = -INUM2/JFOSEQ
                             IQ(KQSP+LQBKD+IICONT) =
     X                       IQ(KQSP+LQBKD+IICONT) - INUM2
                            CALL UCOPY
     X                           (IHNUM2,IQ(KQSP+LQBKD+INSERT),NWN2)
                             ITGLST = ITGLST + NWN2
                             INSERT = INSERT + NWN2
                          ENDIF
                          IF (NWTXT.GT.0) THEN
*--                             Hollerith word count
                             IQ(KQSP+LQBKD+IICONT) =
     X                       IQ(KQSP+LQBKD+IICONT) + JFOSEQ*NWTXT
                            CALL UCOPY
     X                           (IHTXT,IQ(KQSP+LQBKD+INSERT),NWTXT)
                             ITGLST = ITGLST + NWTXT
                             INSERT = INSERT + NWTXT
                          ENDIF
                      ENDIF
*--                     Update the number of words in use
                      IQ(KQSP+LQBKD+MBUSEQ) =
     X                IQ(KQSP+LQBKD+MBUSEQ) + INSERT - II
                      IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) =
     X                IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) + INSERT - II
                  ENDIF
*--                 Memorize the presence or otherwise of a closing '/'
                  IISLSH = ISLASH
              ELSE
*--                 Continuation card
                  IF (NWTXT.EQ.0)                          GO TO 500
                  INSERT = IICONT
                  NWPREV = ITGLST
*--                 Was there already text written?
                  IF (ITGLST.EQ.0) THEN
                      IQ(KQSP+LQBKD+INSERT) = IFOHOQ+JFOSEQ*NWTXT
                      INSERT = INSERT + 1
                      ITGLST = 1
                  ELSE
                      INSERT = INSERT + ITGLST
                      IF (IISLSH.NE.0 .OR. LFORCE.NE.0) THEN
*--                         Line feed desired --> add special word
                          IQ(KQSP+LQBKD+INSERT) = IILFLF
                          IQ(KQSP+LQBKD+IICONT) =
     X                    IQ(KQSP+LQBKD+IICONT) + JFOSEQ
                          INSERT = INSERT + 1
                          ITGLST = ITGLST + 1
                      ENDIF
                      IQ(KQSP+LQBKD+IICONT) =
     X                IQ(KQSP+LQBKD+IICONT) + JFOSEQ*NWTXT
                  ENDIF
                  CALL UCOPY(IHTXT,IQ(KQSP+LQBKD+INSERT),NWTXT)
                  ITGLST = ITGLST + NWTXT
                  INSERT = INSERT + NWTXT
*--                 Memorize the presence or otherwise of a closing '/'
                  IISLSH = ISLASH
*--                 Update the number of words in use
                  IQ(KQSP+LQBKD+MBUSEQ) = IQ(KQSP+LQBKD+MBUSEQ) +
     X                                  INSERT - (IICONT+NWPREV)
                  IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) =
     X            IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) +INSERT-(IICONT+NWPREV)
              ENDIF
                                                           GO TO 500
          ELSEIF(CARD(3:3).EQ.'/') THEN
*----------     It is a end of sequence indicator -> decode according
*----------     characters 4 and 5 and take appropriate action
              ICONT  = 0
              CALL DZDEOS(*998)
              IF (IREPLV.LT.IREPLO) THEN
*--                 Decrease of repetition level
                  IPAT  = 0
*--                 Set the End Of Sequence bit
                  IQ(KQSP+LQBKD+INSERT+MBPATQ) = IBSET(IPAT,IBEOSQ)
*--                 Introduce the tag identifier
                  CALL CBYT(ITGREQ,1,
     X            IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHIDQ+1,NCHIDQ)
*                  CALL MVBITS(ITGREQ,0,NCHIDQ,
*     X                        IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHIDQ)
*--                   and introduce the repetition level
                  CALL CBYT(IREPLV,1,
     X            IQ(KQSP+LQBKD+INSERT+MBPATQ),IRPLVQ+1,NRPLVQ)
*                  CALL MVBITS(IREPLV,0,NRPLVQ,
*     X                        IQ(KQSP+LQBKD+INSERT+MBPATQ),IRPLVQ)
*--                 Get the self-describing sector right
                  IQ(KQSP+LQBKD+INSERT) = IFOINQ + JFOSEQ*(NBEOSQ-1)
                  INSERT = INSERT + NBEOSQ
                  IREPLO = IREPLV
*--                 Update the number of words in use
                  IQ(KQSP+LQBKD+MBUSEQ) = IQ(KQSP+LQBKD+MBUSEQ) + NBEOSQ
                  IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) =
     X            IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) + NBEOSQ
              ENDIF
          ELSEIF(CARD(3:3).EQ.'=') THEN
*----------     Multiple choice for single entry
              ICONT  = 0
*--             Check whether choice possible for entry
              IF(IBKTAC(MBKAHQ,IDTAG).EQ.IBKAYQ.OR.ISTATE.NE.ISGENQ)THEN
*--                 OK -- Set choice bit
                  IPAT  = IQ(KQSP+LQBKD+ISTEER+MBPATQ)
                  IQ(KQSP+LQBKD+ISTEER+MBPATQ) = IBSET(IPAT,ICHBTQ)
*--                 Parse the input first input line
              ELSE
*--                 Choice not allowed
                  WRITE(LUNOUT,'(''0'',I6,3X,A,/,
     X                  '' ?? No choice allowed for present tag -- '',
     X                  ''scan abandoned'')') ICARD,CARD
                                                           GO TO 998
              ENDIF
*--             Decode the first line of input for the multiple choice
              IF (ISTATE.EQ.ISGENQ) THEN
                  IDECOD = IBKTAC(MBKAGQ,IDTAG)
                  IF (IDECOD.EQ.IBKANQ) THEN
*--                     Text only
                      CALL DZD00T
                  ELSEIF(IDECOD.EQ.IBKAMQ) THEN
*--                     Number field
                      CALL DZD0NT
                  ELSE
*--                     Standard tag field
                      CALL DZD0TT
                  ENDIF
              ELSE
*--                 Standard tag field
                  CALL DZD0TT
              ENDIF
*--             Memorize the entry's address (for continuation)
              IICONT = INSERT
*--             Nb. of words for tag field (Hollerith word count)
              IQ(KQSP+LQBKD+INSERT) = IFOHOQ+JFOSEQ*NWTENQ
              CALL UCOPY(IHTAG,IQ(KQSP+LQBKD+INSERT+1),NWTENQ)
              ITGLST = NWTENQ + 1
              INSERT = INSERT + ITGLST
              IF (NWTXT.GT.0) THEN
*--             Hollerith word count
                  IQ(KQSP+LQBKD+IICONT) = IQ(KQSP+LQBKD+IICONT)
     X                                + JFOSEQ*NWTXT
                  CALL UCOPY(IHTXT,IQ(KQSP+LQBKD+INSERT),NWTXT)
                  ITGLST = ITGLST + NWTXT
                  INSERT = INSERT + NWTXT
              ENDIF
*--             Update the number of words in use
              IQ(KQSP+LQBKD+MBUSEQ) =IQ(KQSP+LQBKD+MBUSEQ)+INSERT-IICONT
              IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) =
     X        IQ(KQSP+LQBKD+MBGENQ+ISTATE-1) + INSERT - IICONT
*--             Memorize the presence or otherwise of a closing '/'
              IISLSH = ISLASH
          ELSE
*----------     Abnormal end (unrecognized card)
*----------     Generate end of bank descriptor exit
              WRITE(LUNOUT,'(''0?? ? The following card interrupts a '',
     X         ''valid bank descriptor sequence since it is not of '',
     X         ''the form *B. or *B/ in columns 1/3'',/,1X,A)') CARD
                                                           GO TO 600
          ENDIF
  500 CONTINUE
 
*--             End of present bank descriptor
*--             Finalize bank entry and return
 
  600 CONTINUE
*--      NORMAL EXIT
                                                           GO TO 999
*--      ABNORMAL END
  998 CONTINUE
      WRITE(LUNOUT,'(A,A)')' Previous error occured in: ',CIDBK
      RETURN 1
  999 RETURN IRETUR
      END
