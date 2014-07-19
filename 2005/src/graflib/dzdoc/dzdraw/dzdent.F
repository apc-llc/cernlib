*
* $Id: dzdent.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdent.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDENT
************************************************************************
*.                                                                     *
*...DZDENT outputs the link,data and status bit information for a bank *
*.                                                                     *
*.  DZDENT is a daugther routine of DZDOCO                             *
*.  It prints the information about the link, status bit and data      *
*.  part of a given bank identifier                                    *
*.                                                                     *
*. BANKS  R : QBKD                                                     *
*. CALLS    : DZDLIN,INDXBC                                            *
*. CALLED   : DZDOCO                                                   *
*. COMMON   : DZDOCC,DZDTAP,MZCN                                       *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 2.03(47)  / 11 Oct 1987                                  *
*.                                                                     *
*. UPDATE   : 21 Mar 1988                                              *
*. BY       : O.Schaile                                                *
*.            Allow return of all described links if B-option is       *
*.            given (rather then only announced one)                   *
*.            Use link in link area (LBQBKD, LQBKD) for all references *
*.            Suppress printing at various places if B-option given    *
*.            3 Dec 88
*.            simplify SGML, remove all SGML from this routine
*.            21-Nov-95 PN:  Z option supress data description dump    *
*.**********************************************************************
C     SAVE
 
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
#include "dzdoc/linout.inc"
      LOGICAL BTEST, LTHERE, RTHERE,BTHERE,DTHERE
*--     General information
      CHARACTER CFORM*4
#include "dzdoc/docparq.inc"
#include "dzdoc/dzdocobk.inc"
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"
 
      LTHERE=.FALSE.
      RTHERE=.FALSE.
      BTHERE=.FALSE.
      DTHERE=.FALSE.
   10 CONTINUE
 
*--     Loop over the link, status bit and data information
 
      II     = NBHEAQ
      IF (IFLOPT(MPOSBQ).NE.0) THEN
*--         Link description bank option
*--           Number of links announced
          NLLINK = IQ(KQSP+LBQBKD+MONLQ)
          NSLINK = IQ(KQSP+LBQBKD+MONSQ)
          NLLL = MAX(NLLINK,NSLINK)
*--           Get the number ow words required in the bank
          NLPUSH = NLLL + MOLK0Q - IQWND(KQSP+LBQBKD)
*--           If number of link > 0 -- Push bank, otherwise noopt
 
*--- OTTO    allow return of non announced links
 
*--          CALL DZVERI('Before MZPUSH',0,'CFLSU')
         IF (NLLL.GE.0) CALL MZPUSH(0,LBQBKD,0,NLPUSH,'I')
*--      PRINT '('' NLLINK NLPUSH '',2I5)',NLLINK,NLPUSH
*--      CALL DZSHOW('After PUSH',0,L,' ',0,0,0,0)
*--           Local working variables
*--            0th address and max. nb. entries in link description bank
*          MBLK0  = KQSP + LBQBKD + MOLK0Q
          NBLMAX = IQWND(KQSP+LBQBKD) - MOLK0Q
 
          IHLPUT = 0
 
      ENDIF
      ITAG   = IOTAGQ
      IF (IFLOPT(MPOSIQ).NE.0) THEN
         IREP   = 0
      ELSE
         IREP   = 5
      ENDIF
 
      DO 120 I=MBLINQ,MBDATQ
          IF (IFLOPT(MPOSBQ).NE.0.AND.I.NE.MBLINQ)         GO TO 120
          II     = II + IQ(KQSP+LQBKD+I-1)
          INSERT = II + 1
          IREPLO = 0
          INDENT = 0
          IF (I.EQ.MBLINQ .AND. IFLOPT(MPOSBQ).EQ.0) THEN
              IF (IQ(KQSP+LQBKD+I).GT.0) THEN
                 IF (IFLOPT(MPOSIQ).NE.0) THEN
                     WRITE(LUNUSR,'(''*B.LINK'')')
                     LTHERE=.TRUE.
                 ELSE
                     WRITE(LUNUSR,'
     +               (T15,''---------- Description of the '',
     +               ''links  ----------'')           ')
                 ENDIF
             ELSE
C--              No link description
                                                           GO TO 120
             ENDIF
 
          ELSEIF (I.EQ.MBRLIQ) THEN
              IF (IQ(KQSP+LQBKD+I).GT.0) THEN
                 IF (IFLOPT(MPOSIQ).NE.0) THEN
                     RTHERE=.TRUE.
                     IF(LTHERE)THEN
                        WRITE(LUNUSR,'(''*B/LINK'')')
                        LTHERE=.FALSE.
                     ENDIF
                     WRITE(LUNUSR,'(''*B.RLINK'')')
                 ELSE
                     WRITE(LUNUSR,'
     +               (T15,''---------- Description of the '',
     +               ''Reference links  ----------'')           ')
                 ENDIF
             ELSE
C--              No ref link description
                                                           GO TO 120
             ENDIF
 
          ELSEIF (I.EQ.MBBITQ) THEN
            IF (IQ(KQSP+LQBKD+I).GT.0) THEN
                 IF (IFLOPT(MPOSIQ).NE.0) THEN
                     WRITE(LUNUSR,'(''*B.BI'')')
                     BTHERE=.TRUE.
                     IF(LTHERE)THEN
                        WRITE(LUNUSR,'(''*B/LINK'')')
                        LTHERE=.FALSE.
                     ENDIF
                     IF(RTHERE)THEN
                        WRITE(LUNUSR,'(''*B/RLINK'')')
                        RTHERE=.FALSE.
                     ENDIF
                 ELSE
                     WRITE(LUNUSR,'
     +               (T15,''---------- Description of the '',
     +               ''status bits ----------'')      ')
                 ENDIF
            ELSE
C--               No status bit description
                                                           GO TO 120
            ENDIF
          ELSEIF (I.EQ.MBDATQ) THEN
            IF (IQ(KQSP+LQBKD+I).GT.0 .AND. IFLOPT(MPOSZQ).EQ.0) THEN
                 IF (IFLOPT(MPOSIQ).NE.0) THEN
                     WRITE(LUNUSR,'(''*B.DATA'')')
                     DTHERE=.TRUE.
                     IF(LTHERE)THEN
                        WRITE(LUNUSR,'(''*B/LINK'')')
                        LTHERE=.FALSE.
                     ENDIF
                     IF(RTHERE)THEN
                        WRITE(LUNUSR,'(''*B/RLINK'')')
                        RTHERE=.FALSE.
                     ENDIF
                     IF(BTHERE)THEN
                        WRITE(LUNUSR,'(''*B/BI'')')
                        BTHERE=.FALSE.
                     ENDIF
                 ELSE
                     WRITE(LUNUSR,'
     +               (T15,''---------- Description of the '',
     +               ''data words   ----------'')     ')
                 ENDIF
            ELSE
C--            No data description
                                                           GO TO 120
            ENDIF
          ENDIF
   20     IF (INSERT.GT.II+IQ(KQSP+LQBKD+I))               GO TO 100
          JTAG   = IQ(KQSP+LQBKD+INSERT+MBPATQ)
          NWTAG  = IBITS(JTAG,ICHTGQ,NCHTGQ)
          ICHOIC = IBITS(JTAG,ICHBTQ,1)
          IDTAG  = IBITS(JTAG,ICHIDQ,NCHIDQ)
          IREPLV = IBITS(JTAG,IRPLVQ,NRPLVQ)
          IF (IREPLO.LT.IREPLV) THEN
*----           Repetition field descriptor
*--                  Start of higher level
               IF (IFLOPT(MPOSIQ).NE.0) THEN
                  COUT   = '*B.REP '
                  INDENT = 8
               ELSE
                  INDENT = IREPLO*INDENQ
                  COUT   = ' '
                  COUT(INDENT+2:)='--REP level='//
     +                         CHAR(ICHAR('0')+IREPLV)
               ENDIF
 
*1810               IF    (IREPLO.EQ.0)THEN
*1810                  COUT(INDENT+2:)='-- Do I = 1,'
*1810               ELSE IF(IREPLO.EQ.1)THEN
*1810                  COUT(INDENT+2:)='-- Do K = 1,'
*1810               ELSE
*1810                  COUT(INDENT+2:)='-- Do J = 1,'
*1810               ENDIF
              IREPLO = IREPLV
              INUM1  = IQ(KQSP+LQBKD+INSERT+MBIX1Q)
              IF (INUM1.GT.0) THEN
*--               Given as real number
                  WRITE(COUT(ITAG+IREP+INDENT:),'(I8)') INUM1
              ELSEIF (INUM1.EQ.INUINQ) THEN
*--                 Indefinite (variable) number
                  COUT(ITAG+IREP+INDENT:) = 'infinite'
              ELSE
*--                 Information is in form of Hollerith text
                  NWTAG = -INUM1/JFOSEQ
              ENDIF
*--             Skip I self-describing sector
              INSERT = INSERT + NBDSCQ
*--             If not Hollerith self-describing sector -- SKIP
              IF (MOD(IQ(KQSP+LQBKD+INSERT),JFOSEQ).NE.IFOHOQ) THEN
                  IF (NWTAG.GT.0) THEN
*--                     Inconsistency: Tag announced and non present
                      WRITE(LUNUSR,'(''0?? DZDENT: Inconsistency: '',
     +                ''Tag information announced and non present'')')
                                                           GO TO 130
                  ENDIF
                  IF(IFLOPT(MPOSBQ).EQ.0)THEN
*                     IF (INUM1.NE.0)
                      WRITE(LUNUSR,'(A)') COUT
                  ENDIF
                                                           GO TO 20
              ENDIF
*--             Total number of Hollerith words
              NWHOLL = IQ(KQSP+LQBKD+INSERT)/JFOSEQ
              INSERT = INSERT + 1
*--             When there is tag info
              IF (NWTAG.GT.0) THEN
*--                Numbers given as Hollerith text
                 INLINE=ITAG+IREP+INDENT
                 CALL UHTOC(IQ(KQSP+LQBKD+INSERT),4,
     +                      COUT(INLINE:),NWTAG*4)
                 INSERT = INSERT + NWTAG
                 NWHOLL = NWHOLL - NWTAG
              ENDIF
*--             Is there some text left
              IF (NWHOLL.GT.0) THEN
*--                 More text in the buffer
                  INLINE = INLINE+NWTAG*4
*                  INLINE = IOENTQ + IREP + INDENT
                  IEWORK = 0
                  NOUTF  = NOUTQ - INLINE + 1
                  CALL DZDLIN
              ELSE
                  IF(IFLOPT(MPOSBQ).EQ.0)WRITE(LUNUSR,'(A)') COUT
              ENDIF
 
*------------------ End of loop over Hollerith text
              INDENT = IREPLV*INDENQ
          ELSEIF (IREPLO.GT.IREPLV) THEN
*----           End of repetition field
              IF (IFLOPT(MPOSBQ) .EQ. 0)THEN
*--              Level goes down --> close present level
                  INDENT = IREPLV*INDENQ
                  IF (IFLOPT(MPOSIQ).NE.0) THEN
                     COUT   = '*B/REP '
                  ELSE
                     COUT   = ' '
                     COUT(INDENT+2:)='--REP level='//
     +                       CHAR(ICHAR('0')+IREPLO)//' -- End --'
*1810               COUT(INDENT+2:)='-- End Do -----------------------'
                 ENDIF
                 WRITE (LUNUSR,'(A)') COUT
              ENDIF
              IREPLO = IREPLV
*--             Cross check on EOFS flag
              IF (.NOT.BTEST(JTAG,IBEOSQ)) THEN
                   WRITE (LUNUSR,'(''0??  DZDENT- Illegal repetition''
     +                  ,'' level counting (only 1 item allowed'')')
*                  WRITE (LUNOUT,'(''0??  DZDENT -- Illegal repetition''
*     X                ,'' level counting (only 1 item allowed'')')
 
                                                          GO TO 130
              ELSE
*--                 Skip I self-describing sector
                  INSERT = INSERT + NBEOSQ
              ENDIF
          ELSE
*----           Entry documentation
              INUM1  = IQ(KQSP+LQBKD+INSERT+MBIX1Q)
              INUM2  = IQ(KQSP+LQBKD+INSERT+MBIX2Q)
              IF (INUM1.GT.0) THEN
*--               Get number of characters in INUM1
                  ICH    = INUM1
                  DO 30  IW1=1,8
                      ICH = ICH/10
                      IF (ICH.EQ.0)                        GO TO 40
   30             CONTINUE
                  IW1 = 8
   40         ENDIF
              IF (INUM2.GT.0) THEN
*--               Get number of characters in INUM2
                  ICH    = INUM2
                  DO 50  IW2=1,8
                      ICH = ICH/10
                      IF (ICH.EQ.0)                        GO TO 60
   50             CONTINUE
                  IW2 = 8
   60         ENDIF
*--             Indent for each new repetition level
              COUT   = ' '
              IF (IFLOPT(MPOSIQ).NE.0) THEN
                 COUT='*B.'
                 INLINE=3
                 IW1=IW1-1
              ELSE
                 COUT   = ' '
                 INLINE = INDENT*INDENQ
              ENDIF
              IF (INUM2.EQ.0) THEN
*--                 Case of only one number given
                  IF (INUM1.GE.0) THEN
*--                   Given as real number
                      CFORM  = '(I'//CHAR(ICHAR('1')+IW1)//')'
                      WRITE(COUT(INLINE+1:),CFORM) INUM1
                      INLINE = INLINE + IW1 + 1
                  ELSEIF (INUM1.EQ.INUINQ) THEN
*--                     Indefinite (variable) number
                      INLINE = INLINE + 1
                      COUT(INLINE:) = '*'
                  ENDIF
              ELSE
*--                 Case of two numbers
                  IF (INUM1.GE.0) THEN
*--                   Given as real number
                      CFORM  = '(I'//CHAR(ICHAR('1')+IW1)//')'
                      WRITE(COUT(INLINE+1:),CFORM) INUM1
                      INLINE = INLINE + IW1 + 1
                  ELSEIF (INUM1.EQ.INUINQ) THEN
*--                     Indefinite (variable) number
                      INLINE = INLINE + 1
                      COUT(INLINE:) = '*'
                  ENDIF
                  IF (INUM2.GE.0) THEN
*--                   Given as real number
                      INLINE = INLINE + 1
                      COUT(INLINE:INLINE) = '-'
                      CFORM  = '(I'//CHAR(ICHAR('0')+IW2)//')'
                      WRITE(COUT(INLINE+1:),CFORM) INUM2
                      INLINE = INLINE + IW2
                  ELSEIF (INUM2.EQ.INUINQ) THEN
*--                     Indefinite (variable) number
                      COUT(INLINE+1:INLINE+2) = '-*'
                      INLINE = INLINE + 2
                  ENDIF
              ENDIF
*--             Skip I self-describing sector
              INSERT = INSERT + NBDSCQ
*--             If not Hollerith self-describing sector -- SKIP
              IF (MOD(IQ(KQSP+LQBKD+INSERT),JFOSEQ).NE.IFOHOQ) THEN
*                  WRITE(LUNOUT,'(''0?? DZDENT -- Incomplete data'')')
                  WRITE(LUNUSR,'(''0?? DZDENT -- Incomplete data'')')
                  WRITE(LUNUSR,'(A,A4)')' Previous error occured in',
     +                      IQ(KQSP+LBQBKD+MOIDHQ)
                                                           GO TO 20
              ENDIF
*--             Total number of Hollerith words
              NWHOLL = IQ(KQSP+LQBKD+INSERT)/JFOSEQ
              IITAG  = INSERT + 1
              INSERT = IITAG
              IF (INUM2.EQ.0) THEN
*--                 Case of only one number given
                  IF (INUM1.LE.-JFOSEQ) THEN
*--                     Information is in form of Hollerith text
                      NWN1   = -INUM1/JFOSEQ
                      CALL UHTOC(IQ(KQSP+LQBKD+INSERT+NWTAG),4,
     +                           COUT(INLINE+1:),NWN1*4)
                      INLINE = INDXBC(COUT(:INLINE+NWN1*4),' ')
                      INSERT = INSERT + NWN1
                      NWHOLL = NWHOLL - NWN1
                  ENDIF
              ELSE
*--                 Case of two numbers
                  IF (INUM1.LE.-JFOSEQ) THEN
*--                     Information is in form of Hollerith text
                      NWN1   = -INUM1/JFOSEQ
                      CALL UHTOC(IQ(KQSP+LQBKD+INSERT+NWTAG),4,
     +                           COUT(INLINE+1:),NWN1*4)
                      INLINE = INDXBC(COUT(:INLINE+NWN1*4),' ')
                      INSERT = INSERT + NWN1
                      NWHOLL = NWHOLL - NWN1
                  ENDIF
                  IF (INUM2.LE.-JFOSEQ) THEN
*--                     Information is in form of Hollerith text
                      INLINE = INLINE + 1
                      COUT(INLINE:INLINE) = '-'
                      NWN2   = -INUM2/JFOSEQ
                      CALL UHTOC(IQ(KQSP+LQBKD+INSERT+NWTAG),4,
     +                           COUT(INLINE+1:),NWN2*4)
                      INLINE = INDXBC(COUT(:INLINE+NWN2*4),' ')
                      INSERT = INSERT + NWN2
                      NWHOLL = NWHOLL - NWN2
                  ENDIF
              ENDIF
              INLINE = INDENT + ITAG
 
*--             Special case for the link descriptor bank
 
              IF (IFLOPT(MPOSBQ).EQ.0)                     GO TO 90
*--                Update pointer in input bank and test tag information
                  INSERT = INSERT + NWHOLL
                  LENBKD = IQWND(LBQBKD+KQSP)
                  IF (NWTAG.LE.0)                          GO TO 20
*--                Now cases for link numbers
                  IF (INUM1.LE.0)                          GO TO 20
                  IF (INUM2.LT.INUINQ) THEN
                                                           GO TO 20
                  ELSEIF (INUM2.EQ.INUINQ) THEN
*--                     Undefined --> Fill bank up to end and exit
                      DO 70  IINUM=INUM1,NBLMAX
   70                 IQ(KQSP + LBQBKD + MOLK0Q+IINUM) =
     +                                      IQ(KQSP+LQBKD+IITAG)
                                                           GO TO 120
                  ELSEIF (INUM2.EQ.0) THEN
*--                     Only one number given - Inside allowed offsets?
                      IF(INUM1+MOLK0Q .GT. LENBKD)THEN
                          NLPUSH=INUM1+MOLK0Q - LENBKD
                          CALL MZPUSH(0,LBQBKD,0,NLPUSH,'I')
                      ENDIF
                      IQ(KQSP + LBQBKD + MOLK0Q+INUM1)=
     +                                      IQ(KQSP+LQBKD+IITAG)
                      IF(INUM1 .GT. IHLPUT)IHLPUT = INUM1
                  ELSE
*--                     Range of numbers given - Inside allowed offsets?
                      DO 80  IINUM=MIN(INUM1,NBLMAX),MIN(INUM2,NBLMAX)
   80                 IQ(KQSP + LBQBKD + MOLK0Q+IINUM) =
     +                                     IQ(KQSP+LQBKD+IITAG)
                  ENDIF
                                                           GO TO 20
   90         CONTINUE
*--             When there is tag info  <===========
              IF (NWTAG.GT.0) THEN
*--                 Numbers given as Hollerith text
                  CALL UHTOC(IQ(KQSP+LQBKD+IITAG),4,
     +                       COUT(INLINE:),MIN(NENTIQ-2,NWTAG*4))
*1810
                  IF(INDEX(COUT,'L:').NE.0)THEN
                     IF (IFLOPT(MPOSIQ).NE.0) THEN
                        COUT(1:4)  = '*B.*'
                     ELSE
                        WRITE(LUNUSR,'(A)')' '
                        COUT(1:INDEX(COUT,'L:')-1)='--Label:'
                     ENDIF
                  ENDIF
                  INSERT = INSERT + NWTAG
*--                 Is there some text left
                  NWHOLL = NWHOLL - NWTAG
              ENDIF
*--             Remaining text
              IF (NWHOLL.GT.0) THEN
*--                 Copy text according to output type desired
                  INLINE = IOENTQ + INDENT
                  IF (NWHOLL.EQ.1.AND.IQ(KQSP+LQBKD+INSERT).EQ.IDEMTX)
     +                                                           THEN
*--                     Repetition of the previous line
                      COUT(INLINE:) = '  idem'
                      INSERT = INSERT + 1
                      NWHOLL = 0
                      IF(IFLOPT(MPOSBQ).EQ.0)WRITE(LUNUSR,'(A)') COUT
                  ELSE
                      IEWORK = 0
                      NOUTF  = NOUTQ  - INLINE + 1
*--                     General case with text
                      CALL DZDLIN
                  ENDIF
              ELSE
*--             For tags only
                  IF(IFLOPT(MPOSBQ).EQ.0)WRITE(LUNUSR,'(A)') COUT
*------------------ End of field with text information
              ENDIF
*--------------- Multiple choice present ?
              IF (ICHOIC.NE.0) THEN
*--                 Still more text to read?
                  IF (MOD(IQ(KQSP+LQBKD+INSERT),JFOSEQ).EQ.IFOHOQ) THEN
*--                   Indent for each new repetition level
                      COUT   = ' '
                      INLINE = INDENT + ITAG
*--                     Total number of Hollerith words
                      NWHOLL = IQ(KQSP+LQBKD+INSERT)/JFOSEQ
                      IITAG  = INSERT + 1
                      INSERT = IITAG
*--  ==============     Alternative choice for entry      ==============
                                                           GO TO 90
*--          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                  ENDIF
              ENDIF
*-------------- End of entry (repetition or information tag) [below 110]
          ENDIF
                                                           GO TO 20
*--         Close the description level if needed
  100     CONTINUE
          IF(IFLOPT(MPOSBQ) .EQ. 0)THEN
              DO 110 IR=IREPLV,1,-1
                  IF (IFLOPT(MPOSIQ).NE.0) THEN
                     COUT   = '*B/REP '
                     INDENT = 10
                  ELSE
                     INDENT = (IR-1)*INDENQ
                     COUT   = ' '
                     COUT(INDENT+2:)='--REP level='//
     +                            CHAR(ICHAR('0')+IR)//' -- End --'
*1810                COUT(INDENT+2:)='-- End Do -----------------------'
                  ENDIF
                  WRITE (LUNUSR,'(A)') COUT
  110         CONTINUE
          ENDIF
          IF(LTHERE)THEN
             WRITE(LUNUSR,'(''*B/LINK'')')
             LTHERE=.FALSE.
          ENDIF
          IF(RTHERE)THEN
             WRITE(LUNUSR,'(''*B/RLINK'')')
             RTHERE=.FALSE.
          ENDIF
          IF(BTHERE)THEN
             WRITE(LUNUSR,'(''*B/BI'')')
             BTHERE=.FALSE.
          ENDIF
          IF(DTHERE)THEN
             WRITE(LUNUSR,'(''*B/DATA'')')
             DTHERE=.FALSE.
          ENDIF
*---------- End of loop over link/status bits/data info (400)
  120 CONTINUE
 
      GOTO 140
 
  130 CONTINUE
 
      WRITE(LUNOUT,'(A,A4)')' Previous error occured in: ',
     +                      IQ(KQSP+LQBKD+1)
 
  140 CONTINUE
 
*--- OTTO tell highest link number
 
      IQ(LBQBKD+KQSP+5) = IHLPUT
      IQUEST(MOQUEQ) = LBQBKD
 
      END
