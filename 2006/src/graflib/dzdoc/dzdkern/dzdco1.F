*
* $Id: dzdco1.F,v 1.1.1.1 1996/03/04 16:12:56 mclareni Exp $
*
* $Log: dzdco1.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:56  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDCO1(CHKEY,IFLAG,CHOPTI,CHPRE,LUN,IFI,ILI)
      CHARACTER*8 CHKEY,CHBANK,CHOPT
      CHARACTER*(*) CHOPTI,CHPRE
*     IFLAG: 1 make keeps for INTEGER declarations
*            2 continue INTEGER decl
*            3 make keeps for booking +KEEP..
*            4 contiue booking no +KEEP..
*            5 COMMON with header
*            6  continue COMMON
*            7  make keeps for MMIDBK fill bank descriptor v
*            8  continue with  MMIDBK
*           13  gen MZLIFT instead of MZBOOK
*           14  continue MZLIFT
*     CHOPT:  'I'   Integer declarations for booking
*             'C'   Common statements for booking
*             'B'   calls to MZBOOK
*             'L'   Integer declarations for linkoffsets
*             'P'   Parameter statements for linkoffsets
*             'A'   link assigment statements
*             'K'   keep seq for data word offsets
*             'D'   Integer decl for data word offsets
*             'O'   Parameter statements for data word offsets
*             'N'   follow next links
 
      INTEGER KEY(2),KEYI(2)
#include "dzdoc/bkwrp.inc"
*----
      CALL UCTOH(CHKEY,KEY,4,8)
      NC=MIN(LNBLNK(CHOPTI),8)
      CHOPT=CHOPTI(1:NC)
      CALL DZDPIN(KEY,'RBG')
      IF(IQUEST(1) .NE. 0)THEN
           WRITE(LUN,*)'*     ',CHKEY,'  not found'
           GOTO 999
      ENDIF
      LDOC = IQUEST(11)
*      IDHTB=KEY(1)
      CHBANK = CHKEY
      CALL CLTOU(CHBANK)
      ILCP=MAX(INDXBC(CHPRE,' '),1)
 
*     top bank
 
      CALL DZDLFT(LDOC,LLTOP,LT,1)
      IF    (IFLAG.LT.100)THEN
                CALL DZDBK1(CHBANK,LUN,IFLAG)
      ELSEIF(IFLAG.GE.101 .AND. IFLAG.LE.102)THEN
                CALL DZDLO1(CHBANK,CHPRE,LUN,IFLAG-100)
      ELSEIF(IFLAG.GE.103 .AND. IFLAG.LE.104)THEN
                CALL DZDLO1(CHBANK,CHPRE,LUN,IFLAG-100)
      ELSEIF(IFLAG.EQ.105)THEN
                ILCB=INDXBC(CHBANK(1:4),' ')
                ILCU=INDXBC(CHBANK(5:8),' ')+4
                WRITE(LUN,'(A)')'+KEEP,LASS'//CHBANK(1:ILCB)
                IF(CHBANK(5:8).NE.'NONE')WRITE(LUN,'(6X,10A)')
     &          'L',CHBANK(1:ILCB),'=LQ(L',CHBANK(5:ILCU)
     &          ,'-',CHPRE(1:ILCP),CHBANK(1:ILCB),')'
      ELSEIF(IFLAG .EQ. 201)THEN
                NKEEPS = 0
                CALL DZDDOC(0,0,CHBANK,'KD'//CHOPT,
     &          IFI,ILI,LUN,CHPRE,NKEEPS)
      ELSEIF(IFLAG .EQ. 203)THEN
                NKEEPS = 0
                CALL DZDDOC(0,0,CHBANK,'KO'//CHOPT,
     &          IFI,ILI,LUN,CHPRE,NKEEPS)
      ENDIF
 
      IF(INDEX(CHOPT,'T').EQ.0)GOTO 999
 
150   CONTINUE
*
*
*       init links
*
      LORIG = LT
      LUP = LT
      L = LT
 
*       get next down link
 
11      CONTINUE
      JB = LUP - LORIG + 1
      NS = IQ(KQSP+LUP-2)
*
12    CONTINUE
      IF(JB .LE. NS)THEN
 
          IDHD = IQ(KQSP+LDOC+10+JB)
          IF(IDHD .NE. 0)THEN
              KEYI(1) = IDHD
              KEYI(2) = IQ(KQSP+LDOC+7)
              CALL DZDPIN(KEYI,'RGB')
              IF(IQUEST(1) .NE. 0)THEN
                  JB=JB+1
                  GOTO 12
              ELSE
                  LDOC= IQUEST(11)
                  CALL DZDLFT(LDOC,L,LD,-JB)
                  CALL UHTOC(KEYI,4,CHBANK,8)
                  IF    (IFLAG.LT.100)THEN
                      CALL DZDBK1(CHBANK,LUN,IFLAG+1)
                  ELSEIF(IFLAG.GE.101 .AND. IFLAG.LE.104)THEN
                      CALL DZDLO1(CHBANK,CHPRE,LUN,IFLAG-100+1)
                  ELSEIF(IFLAG.EQ.105)THEN
                      ILCB=INDXBC(CHBANK(1:4),' ')
                      ILCU=INDXBC(CHBANK(5:8),' ')+4
                      WRITE(LUN,'(6X,10A)')
     &                'L',CHBANK(1:ILCB),'=LQ(L',CHBANK(5:ILCU)
     &                ,'-',CHPRE(1:ILCP),CHBANK(1:ILCB),')'
                  ELSEIF(IFLAG .EQ. 201)THEN
                      CALL DZDDOC(0,0,CHBANK,'KD'//CHOPT,
     &                IFI,ILI,LUN,CHPRE,NKEEPS)
                  ELSEIF(IFLAG .EQ. 203)THEN
                      CALL DZDDOC(0,0,CHBANK,'KO'//CHOPT,
     &                IFI,ILI,LUN,CHPRE,NKEEPS)
                  ENDIF
*       has it a next bank?
                  IDHN = IQ(KQSP+LDOC+10)
                  IF(IDHN.NE.0 .AND. INDEX(CHOPT,'N').NE.0)THEN
                      KEYI(1) = IDHN
                      CALL DZDPIN(KEYI,'RGB')
                      IF(IQUEST(1).EQ.0)THEN
                          LDOC=IQUEST(11)
                          CALL UHTOC(KEYI,4,CHBANK,8)
                          CALL DZDLFT(LDOC,LD,LN,0)
                          IF    (IFLAG.LT.100)THEN
                              CALL DZDBK1(CHBANK,LUN,IFLAG+1)
                          ELSEIF(IFLAG.GE.101 .AND. IFLAG.LE.104)THEN
                              CALL DZDLO1(CHBANK,CHPRE,LUN,IFLAG-99)
                          ELSEIF(IFLAG.EQ.105)THEN
                              ILCB=INDXBC(CHBANK(1:4),' ')
                              ILCU=INDXBC(CHBANK(5:8),' ')+4
                              WRITE(LUN,'(6X,10A)')
     &                        'L',CHBANK(1:ILCB),'=LQ(L',
     &                        CHBANK(5:ILCU)
     &                        ,'-',CHPRE(1:ILCP),CHBANK(1:ILCB),')'
                          ELSEIF(IFLAG .EQ. 201)THEN
                              CALL DZDDOC(0,0,CHBANK,'KD'//CHOPT,
     &                        IF1,IL1,LUN,CHPRE,NKEEPS)
                          ELSEIF(IFLAG .EQ. 203)THEN
                              CALL DZDDOC(0,0,CHBANK,'KO'//CHOPT,
     &                        IF1,IL1,LUN,CHPRE,NKEEPS)
                          ENDIF
                      ENDIF
                  ENDIF
              ENDIF
          ELSE
              JB=JB+1
              GOTO 12
          ENDIF
 
*         go down
 
          LUP = LD
          LORIG = LD
          L = LD
          GOTO 11
      ENDIF
 
*       look if it is end of a linear structure
*       i.e. origin and up link are in different banks
 
13      CONTINUE
      LUP = LQ(KQSP+L+1)
      LORIG = LQ(KQSP+L+2)
 
*       look if back at top bank
 
      IF(LUP .LE. 1) GOTO 14
*      IF(IQ(KQSP+LUP-4).EQ.IDHUP .AND. IQ(KQSP+L-4).EQ.IDHTB)GOTO 14
 
      NSU = IQ(KQSP+LUP-2)
      IF(LORIG .GE. LUP .OR. LORIG .LT. LUP-NSU)THEN
 
*             its different, step back
 
            L = LORIG
            GOTO 13
      ENDIF
 
*       go up
 
      L = LUP
      LUU = LQ(KQSP+L+1)
      IF(LUU .LE. 2)THEN
          KEYI(2) = KEY(2)
      ELSE
          KEYI(2) = IQ(KQSP+LUU-4)
      ENDIF
      KEYI(1) = IQ(KQSP+L-4)
      CALL DZDPIN(KEYI,'RGB')
      LDOC = IQUEST(11)
      GOTO 11
 
*       all done, but top bank
 
14    CONTINUE
999   CONTINUE
      IF(LLTOP.NE.0)THEN
         CALL MZDROP(0,LLTOP,'L')
         LLTOP=0
      ENDIF
      END
 
********************************************************************
