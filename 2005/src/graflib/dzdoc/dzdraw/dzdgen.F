*
* $Id: dzdgen.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdgen.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGEN
************************************************************************
*.                                                                     *
*...DZDGEN outputs the general information for a given bank identifier *
*.                                                                     *
*.  DZDGEN is a daughter routine of DZDOCO                             *
*.  It handles the first part (general information) for a given bank   *
*.  by printing (SGML, Line Printer output) or creating the link       *
*.  descriptor bank QBLK.                                              *
*.                                                                     *
*. BANKS  L : QBLK                                                     *
*. BANKS  R : QBKD                                                     *
*. CALLS    : DZDLIN                                                   *
*. CALLED   : DZDOCO                                                   *
*. COMMON   : DZDOCC,DZDTAP,MZCN,MZLIFT                                *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 2.03(36)  / 27 Sep 1987                                  *
*.                                                                     *
*. UPDATE   : 21 Mar 1988                                              *
*. BY       : O.Schaile                                                *
*.            Use link in link area (LBQBKD, LQBKD) for all references *
*.            Suppress printing at various places if B-option given    *
*.**********************************************************************
C     SAVE
 
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
#include "dzdoc/linout.inc"
*--     Link description bank MZLIFT vector
*-- OTTO MBKBKD => 7 dim
      INTEGER MBKBKD(7)
#include "dzdoc/dzdocobk.inc"
*--     Text and order for general information
      PARAMETER (NNGENQ=14)
      CHARACTER COGEN(NNGENQ)*10,CIGEN(NNGENQ)*2
      INTEGER   IIGEN(NNGENQ),IOBGEN(NNGENQ)
*--     Additional offset for *REP card on output
      PARAMETER (IREPQ=4)
#include "dzdoc/docparq.inc"
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"
 
*--     Order and text for the various general information items
      DATA IIGEN /0,ITGNIQ,ITGAUQ,ITGVEQ,ITGSTQ,ITGDVQ,ITGNLQ,
     +            ITGNSQ,ITGNDQ,ITGNXQ,ITGUPQ,ITGORQ,ITGIOQ,ITGNZQ/
      DATA COGEN /' Bank IDH',' NumericId',' Author',' Version',
     +            ' Store',' Division',
     +            ' NL',' NS',' ND',' Next',' Up',' Origin',
     +            ' IO-Charac',' NZERO'  /
      DATA CIGEN /'.','NI','AU','VE','ST','DV',
     +            'NL','NS','ND','NX','UP','OR',
     +            'IO','NZ'/
 
      DATA IOBGEN / MOIDHQ,0,0,0,0,0,MONLQ,
     +              MONSQ,MONDQ,MONXQ,MOUPQ,MOORQ,
     +              0,0/
 
      DATA MBKBKD/0,0,0,50,0,0,0/ , IFIRST/0/
 
   10 CONTINUE
*      ITAG   = 0
      IF (IFIRST.EQ.0) THEN
*--        Initialization of the name constants
*--         Get the bank NAME vector for the documenation banks
*--         complete by describing its structure as counters at the
*--         bank beginning and then self-describing sectors.
          CALL UCTOH ('QBLK',MBKBKD,4,4)
          CALL MZIOBK(MBKBKD,5,'6I -H')
          IFIRST = 1
      ENDIF
 
      IF (IFLOPT(MPOSBQ).NE.0) THEN
          IQUEST(MOQUEQ) = 0
          CALL MZLIFT(0,LBQBKD,LBQBKD,1,MBKBKD,0)
*          CALL UZERO(IQ,KQSP+LBQBKD+1,KQSP+LBQBKD+MBKBKD(4))
           DO 20 I=KQSP+LBQBKD,KQSP+LBQBKD+MBKBKD(4)-1
   20       IQ(I+1)=0
      ENDIF
 
*--     Loop over the front part of the bank to get the general info
 
      DO 50  IGEN = 1,NNGENQ
          II     = NBHEAQ + 1
*--       Look through the general info to see whether something exists
   30         IF (II-NBHEAQ.GT.IQ(KQSP+LQBKD+MBGENQ))      GO TO 50
*--              Integer self-describing sector?
              IF (MOD(IQ(KQSP+LQBKD+II),JFOSEQ).NE.IFOINQ)THEN
*--                 Not an integer self-describing sector --> skip
                  II     = II + IQ(KQSP+LQBKD+II)/JFOSEQ + 1
                  IF (II-NBHEAQ.GT.IQ(KQSP+LQBKD+MBGENQ)) THEN
                                                           GO TO 50
                  ELSE
                                                           GO TO 30
                  ENDIF
              ENDIF
*--             Integer self-describing sector: look at info id.
              INTAG  = IBITS(IQ(KQSP+LQBKD+II+1),ICHIDQ,NCHIDQ)
              IF (INTAG.NE.IIGEN(IGEN)) THEN
*--                 Not yet the id. needed --> continue
                  II     = II + IQ(KQSP+LQBKD+II)/JFOSEQ + 1
                  IF (II-NBHEAQ.GT.IQ(KQSP+LQBKD+MBGENQ)) THEN
                                                           GO TO 50
                  ELSE
                                                           GO TO 30
                  ENDIF
              ENDIF
*---------- End of loop to locate general information tag (to label 10)
*--         Tag found --> output information
          ITAG   = IOTAGQ
          IF (IFLOPT(MPOSBQ).NE.0) THEN
C-              Skip information if not needed
              IF (IOBGEN(IGEN).EQ.0)                       GO TO 50
          ELSE
              IF (IFLOPT(MPOSIQ).NE.0) THEN
                 COUT   = '*B.'//CIGEN(IGEN)
                 IF(IGEN.EQ.1)ITAG=5
              ELSE
                 COUT   = COGEN(IGEN)
              ENDIF
          ENDIF
*--                    Read information
          INSERT = II
          IF (INTAG.GT.0) THEN
              ITGTYP = IBKTAC(MBKAGQ,INTAG)
          ELSE
*--             Protection for identifier info (INTAG = 0)
              ITGTYP = ITGUPQ
          ENDIF
          IPATAG = IBITS(IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHTGQ,NCHTGQ)
          ICHOIC = IBITS(IQ(KQSP+LQBKD+INSERT+MBPATQ),ICHBTQ,1)
          NWTAG  = 0
          IF (ITGTYP.EQ.IBKAMQ) THEN
*--              Number possible
              IF (IPATAG.EQ.0) THEN
*--                 Explicit number
                  INUM1  = IQ(KQSP+LQBKD+INSERT+MBIX1Q)
                  IF (INUM1.GE.0) THEN
*--                     Given as real number
                      WRITE(COUT(ITAG:),'(I8)') INUM1
                  ELSEIF (INUM1.EQ.INUINQ) THEN
*--                     Indefinite (variable) number
                      COUT(ITAG:) = 'variable'
                  ELSEIF (INUM1.LE.-JFOSEQ) THEN
*--                     Numbers given as Hollerith text
                      NWTAG  = -INUM1/JFOSEQ
                  ENDIF
              ELSE
*--                 Character tag
                  NWTAG = IQ(KQSP+LQBKD+INSERT+MBIX1Q)
                  INUM1 = -1
              ENDIF
*--             In case of link description bank
              IF (IFLOPT(MPOSBQ).NE.0) THEN
 
*-- OTTO 19-MAR-88 if NL,NS,ND  is given as name, mark it variable
 
                  IF  (IIGEN(IGEN) .EQ. ITGNLQ
     +           .OR. IIGEN(IGEN) .EQ. ITGNSQ
     +           .OR. IIGEN(IGEN) .EQ. ITGNDQ) THEN
                      IQ(KQSP+LBQBKD+IOBGEN(IGEN)) = INUM1
                      GOTO 50
                  ENDIF
              ENDIF
          ELSEIF (ITGTYP.EQ.IBKANQ) THEN
*--             Only text
              CONTINUE
          ELSE
*--             Indicate length of tag
              NWTAG = IPATAG
          ENDIF
*--         Skip I self-describing sector
          INSERT = INSERT + NBDSCQ
*--         If not Hollerith self-describing sector -- SKIP
          IF (MOD(IQ(KQSP+LQBKD+INSERT),JFOSEQ).NE.IFOHOQ) THEN
              IF (NWTAG.GT.0) THEN
*--                 Inconsistency: Tag announced and non present
                  WRITE(LUNOUT,'(''0?? DZDGEN: Inconsistency: '',
     +             ''Tag information announced and non present'')')
                  WRITE(LUNUSR,'(''0?? DZDGEN: Inconsistency: '',
     +             ''Tag information announced and non present'')')
                                                           GO TO 999
              ENDIF
              IF (INUM1.NE.0) THEN
*--               If something present
                  IF (IFLOPT(MPOSBQ).NE.0) THEN
                      IQ(KQSP+LBQBKD+IOBGEN(IGEN)) = INUM1
                                                           GO TO 50
                  ELSE
                      IF (IFLOPT(MPOSIQ).NE.0)COUT(1:3)='*B.'
                      WRITE(LUNUSR,'(A)') COUT
                  ENDIF
              ENDIF
                                                           GO TO 50
          ENDIF
*--         Total number of Hollerith words  <==========================
   40     NWHOLL = IQ(KQSP+LQBKD+INSERT)/JFOSEQ
*
          INSERT = INSERT + 1
*--         When there is tag info
          IF (NWTAG.GT.0) THEN
*--             In case of link description bank --> Copy and continue
              IF (IFLOPT(MPOSBQ).NE.0) THEN
 
*-- OTTO 19-MAR-88 if NL,NS,ND  is given as name, mark it variable
 
                IF (MOD(IQ(KQSP+LQBKD+INSERT-1),JFOSEQ).EQ.IFOHOQ
     +           .AND.
     +               (IIGEN(IGEN) .EQ. ITGNLQ
     +           .OR. IIGEN(IGEN) .EQ. ITGNSQ
     +           .OR. IIGEN(IGEN) .EQ. ITGNDQ) ) THEN
                  IQ(KQSP+LBQBKD+IOBGEN(IGEN)) = -1
                      ELSE
                  IQ(KQSP+LBQBKD+IOBGEN(IGEN)) =
     +                                           IQ(KQSP+LQBKD+INSERT)
                ENDIF
                                                           GO TO 50
              ENDIF
*--             Hollerith word count
              CALL UHTOC(IQ(KQSP+LQBKD+INSERT),4,COUT(ITAG:),NWTAG*4)
              INSERT = INSERT + NWTAG
              NWHOLL = NWHOLL - NWTAG
          ENDIF
*--         Is there some text left
          IF(IFLOPT(MPOSBQ) .EQ. 0)THEN
              IF (NWHOLL.GT.0) THEN
*--                 Copy text
                  IF (IFLOPT(MPOSIQ).NE.0
     &           .AND. LENOCC(COUT).LT.IOTAGQ) THEN
                     INLINE = IOTAGQ
                  ELSE
                     INLINE = IOENTQ
                  ENDIF
                  IEWORK = 0
                  NOUTF  = NOUTQ - INLINE + 1
*--                As long as there is text
                  CALL DZDLIN
              ELSE
*--             No text to write --> output line as it is
                  IF (IFLOPT(MPOSIQ).NE.0)COUT(1:3)='*B.'
                  WRITE(LUNUSR,'(A)') COUT
*-------------- End of field for Hollerith text
              ENDIF
          ENDIF
*--------------- Multiple choice present ?
              IF (ICHOIC.NE.0) THEN
*--                 Still more text to read?
                  IF (MOD(IQ(KQSP+LQBKD+INSERT),JFOSEQ).EQ.IFOHOQ) THEN
*--                     Indent for each new repetition level
                      COUT   = ' '
                      ITAG   = IOTAGQ
*--  ==============     Alternative choice for entry      ==============
                                                           GO TO 40
*--          >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
              ENDIF
          ENDIF
*---------- End of loop over general information tags to be printed (100)
   50 CONTINUE
 
*--   Close the description level if needed
 
  999 END
