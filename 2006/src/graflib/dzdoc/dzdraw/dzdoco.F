*
* $Id: dzdoco.F,v 1.1.1.1 1996/03/04 16:13:18 mclareni Exp $
*
* $Log: dzdoco.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:18  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDOCO(LUNBNK,CHBANK,CHOPT)
************************************************************************
*.                                                                     *
*...DZDOCO outputs bank descriptor information in different forms      *
*.                                                                     *
*.  DZDOCO uses the bank description system bank to provide            *
*.  printed information about one or more banks in a store in several  *
*.  user chosen formats.                                               *
*.                                                                     *
*.    SUBROUTINE DZDOCO(LUNBNK,CHBANK,CHOPT)                           *
*.                                                                     *
*. INPUT:                                                              *
*. ------                                                              *
*.   LUNBNK : Unit number of file where the bank descriptor has        *
*.            to be written.                                           *
*.            LUNBNK = 0 means the standard output unit LUNOUT         *
*.   CHBANK : Character variable containing the name of the bank to    *
*.              be documented.                                         *
*.   CHOPT  : Character variable containing the options desired        *
 
*.            'A' : All banks in the given store (CHBANK not used)     *
*.            'O' : List all old cycles                                *
*.            'P' : Printer file (80 colums wide)  -- Default          *
*.            'R' : Read information from RZ file on CWD               *
*.            'S' : SGML tagged output
*.            'H' : highlight headline SGML
*.            'L' : highlight headline Latex
*.            'N' : dont write headline
*.            'I' : Input format for DZDOCM
*.                                                                     *
*. BANKS  R : QBKD,QBKM                                                *
*. CALLS    : DZDENT,DZDGEN,MZCHLS,RZIN,ZBINSR                         *
*. CALLED   : User called (DZMAIN)                                     *
*. COMMON   : DZDOCC,DZDTAP,MZCN                                       *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 2.04(35)  /  1 Dec 1987                                  *
*.                                                                     *
*.**********************************************************************
      CHARACTER CHBANK*(*),CHOPT*(*)
      INTEGER KEYVEC(2)
      CHARACTER*8 CHKEYI
      PARAMETER (NCHDES=60)
      CHARACTER*(NCHDES) CHDESC
      CHARACTER*3 CMONTH(12)
      LOGICAL LWILDC
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
#include "dzdoc/linout.inc"
#include "dzdoc/docparq.inc"
#include "dzdoc/bknuparq.inc"
#include "zebra/bkfoparq.inc"
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bktgdatq.inc"
 
      DATA CMONTH/'Jan','Feb','Mar','Apr','May','Jun',
     +            'Jul','Aug','Sep','Oct','Nov','Dec'/
*------
   10 CONTINUE
 
*     CALL DZSHOW('After bank parsing',0,LDQBKD,'BLV',0,0,0,0)
 
*--     Define which options are desired
      CALL DZDSCO(CHOPT,IFLOPT)
 
      IF (IFLOPT(MPOSSQ).NE.0) THEN
          IFLOPT(MPOSPQ) = 0
      ENDIF
 
*      IF (IFLOPT(MPOSGQ).NE.0) THEN
*     X                   '' Sorry I stop here'')')
*                                                           GO TO 999
*      ENDIF
 
*--     Which output unit is desired
 
      IF(IFLOPT(MPOSGQ) .EQ. 0)THEN
          IF (LUNBNK.EQ.0) THEN
              LUNUSR = LUNOUT
          ELSE
              LUNUSR = LUNBNK
          ENDIF
      ENDIF
 
      NLISTD = 0
 
*---OTTO--- Cant currently use 'R' option with 'A'
 
      IF (  IFLOPT(MPOSAQ).NE.0
     + .AND.IFLOPT(MPOSRQ).EQ.0 ) THEN
 
          IF (LDQBKD.EQ.0) THEN
           WRITE(LUNOUT,'(''0DZDOCO No bank descriptor data structure'',
     +                   /,'' Sorry I stop here'')')
                                                           GO TO 999
          ENDIF
*--         Order the linear chain of bank descriptors
          CALL ZSORTH(0,LDQBKD,MBIDHQ)
 
*--         All known identifiers desired
          LQBKD   = LDQBKD
   20     IF (LQBKD.EQ.0)                                  GO TO 999
*--         Loop over all bank identifiers
              CALL MZCHLS(0,LQBKD)
              IF (IQFOUL.NE.0) THEN
*--                 Invalid bank descriptor bank
                  WRITE(LUNOUT,
     +             '(''0DZDOCO -- Documentation bank at address'',I10,
     +             '' invalid'')')  LQBKD
              ELSE
              WRITE(LUNUSR,'(40('' *''))')
*--                 Treat the general information
                  CALL DZDGEN
*--                 Output the link, status bit and data information
                  CALL DZDENT
              ENDIF
          LQBKD   = LQ(KQSP+LQBKD)
*--             LOOP ===================================================
                                                           GO TO 20
*--             LOOP ===================================================
      ELSE
*--         One particular bank descriptor desired
 
          CALL UCTOH(CHBANK,IDBK,4,4)
 
          IF (IFLOPT(MPOSRQ).EQ.0) GOTO 45
 
*--       Read information in CWD on RZ file
 
*--       --OTTO---- get name of up-bank
 
          LWILDC = .FALSE.
          IF(IFLOPT(MPOSAQ) .EQ. 0)THEN
              KEYVEC(1) = IDBK
              NCHA = LEN(CHBANK)
*--           wildcard ??
              IF(INDEX(CHBANK,'*') .NE. 0)THEN
                  CHKEYI(1:4) = CHBANK(1:4)
                  LWILDC = .TRUE.
              ENDIF
 
*--           if only one word there force wildcard for second
 
              IF(NCHA .GE. 8)THEN
                  CHKEYI(5:8) = CHBANK(5:8)
                  CALL UCTOH(CHBANK(5:),KEYVEC(2),4,4)
              ELSE
                  CHKEYI(1:4) = CHBANK(1:4)
                  CHKEYI(5:8) = '****'
                  LWILDC = .TRUE.
              ENDIF
          ELSE
*--           'A'll option given => wildcard all
 
              CHKEYI = '********'
              LWILDC = .TRUE.
          ENDIF
          NFKEY = 0
 
*-- loop on wildcards
 
   30     CONTINUE
 
          IF(LWILDC)THEN
              CALL DZDWCS(CHKEYI,KEYVEC,NFKEY)
              IF(KEYVEC(1) .EQ. 0)GOTO 999
*             IF(KEYVEC(1) .EQ. 0 .AND. NLISTD .GT. 0)GOTO 999
          ENDIF
 
*---      if all cycles required, get number of cycles
 
          ICYCLE = 1000000
          IF(IFLOPT(MPOSOQ) .NE. 0)THEN
              CALL RZIN(0,LDQBKD,2,KEYVEC,ICYCLE,'C')
              LQBKD = LDQBKD
              NCYCLE = IQUEST(50)
              ICYCLE = 1
          ELSE
              NCYCLE = 0
          ENDIF
 
*--    is graphics option required
 
          IF(IFLOPT(MPOSGQ) .NE. 0)THEN
             WRITE(LUNOUT,*)'Not implemtented',
     +       ' use DZDGTR,DZDGTR instead'
*            CALL DZDOCG(LUNBNK,KEYVEC,CHOPT)
             IF(LWILDC) GOTO 30
          ENDIF
 
*--       loop on cycles, if required
 
   40     CONTINUE
 
          CALL RZIN(0,LDQBKD,2,KEYVEC,ICYCLE,'D')
          LQBKD = LDQBKD
          ICYCLE = ICYCLE + 1
 
*--       if all cycles needed loop until all found
 
          IF(NCYCLE .GT. 0 .AND. IQUEST(1) .NE. 0)THEN
              GOTO 40
          ENDIF
          IF (IQUEST(1).NE.0) LQBKD = 0
          NCYCLE = NCYCLE - 1
          CALL RZDATE(IQUEST(14), IDATE, ITIME,1)
          GOTO 46
 
   45     CONTINUE
*--       Find the information in memory
          IF (LDQBKD.EQ.0) THEN
              WRITE(LUNOUT,'(''0DZDOCO No bank descriptor data '',
     +                    ''structure'',/,'' Sorry I stop here'')')
              GO TO 999
          ENDIF
          LQBKD   = LZFIND(0,LDQBKD,IDBK,MBIDHQ)
 
46        CONTINUE
 
*          WRITE(LUNOUT,'(A,A4)')' DZDOCO for :',KEYVEC(1)
 
          IF (LQBKD.EQ.0) THEN
*--            No bank present for bank identifier
              IF(CHBANK(1:4).NE.'NOTU')THEN
                  WRITE(LUNUSR,'(A,A,A,A)')
     +            ' '//CHBANK(1:4),'/',CHBANK(5:8),' No doc '
                  IF(LUNOUT.NE.LUNUSR)
     +            WRITE(LUNOUT,'(A,A,A,A)')
     +            ' '//CHBANK(1:4),'/',CHBANK(5:8),' No doc '
              ENDIF
                                                           GO TO 999
          ELSE
*--             Get address of documentation bank
              CALL MZCHLS(0,LQBKD)
              IF (IQFOUL.NE.0) THEN
                  WRITE(LUNOUT,
     +            '(''0DZDOCO -- Documentation bank for identifier '',A,
     +             '' invalid'')')  CHBANK
              ELSE
 
*--OTTO----
                  CALL UHTOC(KEYVEC,4,CHBANK,8)
                  IF(IFLOPT(MPOSBQ) .EQ. 0. AND.
     &               IFLOPT(MPOSIQ) .EQ. 0.)THEN
                      IF(IFLOPT(MPOSSQ) .EQ. 0)THEN
                          NCHOLL = (IQ(KQSP+LDQBKD+25)/16 - 1)*4
                          IF(NCHOLL .GT. 0)THEN
                              IF(NCHOLL .GT. NCHDES)NCHOLL = NCHDES
                              CALL UHTOC(IQ(KQSP+LDQBKD+27),
     +                        4,CHDESC,NCHOLL)
                              IF(NCHOLL.LT.NCHDES)
     +                        CHDESC(NCHOLL+1:NCHDES) =' '
                          ELSE
                              CHDESC=' '
                          ENDIF
                          IF(IFLOPT(MPOSNQ).EQ.0)THEN
                              WRITE(LUNUSR,'(/,'' '',78(''-''))')
                              IF(IFLOPT(MPOSHQ).NE.0)THEN
                                  WRITE(LUNUSR,'(A)')'</LISTING>'
                                  WRITE(LUNUSR,'(A)')'<HP2>| '
     +                            //CHBANK(1:4)
     +                            //' | '//CHDESC//'</HP2>'
                                  WRITE(LUNUSR,'(A)')'<LISTING>'
                              ELSEIF(IFLOPT(MPOSLQ).NE.0)THEN
                                  WRITE(LUNUSR,'(A)')'{'//BS//'Large'
                                  WRITE(LUNUSR,'(A)')'| '//CHBANK(1:4)
     +                            //' | '//CHDESC//'}'
                              ELSE
                                  WRITE(LUNUSR,'(A)')'| '//CHBANK(1:4)
     +                            //' | '//CHDESC
                              ENDIF
                          ENDIF
                          IYEAR=IDATE/10000
                          IMONTH = MOD(IDATE/100,100)
                          IDAY = MOD(IDATE,100)
                          IHOUR = ITIME/100
                          IMIN= MOD(ITIME,100)
                          WRITE(LUNUSR,'('' '',46(''-''),
     +                    A,I2,A,A,A,I2,I3,A,I2)')
     +                    ' entered file at ',IDAY,'-',CMONTH(IMONTH),
     +                    '-',IYEAR,IHOUR,':',IMIN
                      ELSE
                          WRITE(LUNUSR,'(A,A,A,A)')
     +                    '<H1 N="',CHBANK(1:4),'">'
                          WRITE(LUNUSR,'(A,A)')'<I1>',CHBANK(1:4)
                          WRITE(LUNUSR,'(A)')'<LISTING>'
                      ENDIF
                  ENDIF
*---/OTTO
*--                 Treat the general information
                  CALL DZDGEN
*--                 Output the link, status bit and data information
                  CALL DZDENT
                  NLISTD = NLISTD + 1
                  IF(IFLOPT(MPOSBQ) .EQ. 0)THEN
                      IF(IFLOPT(MPOSSQ) .NE. 0)THEN
                          WRITE(LUNUSR,'(A)')'</LISTING>'
                      ENDIF
                  ENDIF
              ENDIF
*&&&&&  6-Apr-92
              CALL MZDROP(0,LQBKD,'L')
               LQBKD=0
 
*-- look for next cycle
 
              IF(NCYCLE .GT. 0)GOTO 40
 
*-- wild card search ??
 
              IF(LWILDC) GOTO 30
          ENDIF
      ENDIF
 
  999 END
