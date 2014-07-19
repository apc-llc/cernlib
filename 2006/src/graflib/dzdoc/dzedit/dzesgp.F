*
* $Id: dzesgp.F,v 1.2 1997/03/14 14:20:37 mclareni Exp $
*
* $Log: dzesgp.F,v $
* Revision 1.2  1997/03/14 14:20:37  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:13  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZESGP(LUSGML,LUPPIC,LUNOUT,IOPT)
#include "dzesgpkeep.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      CHARACTER*7 CPNAME
      CHARACTER*24 CCINT
      CHARACTER*80 CTEXT
      CHARACTER*100 CTEMP
#include "dzeindex.inc"
 
      LUOUT = LUNOUT
 
*     units for PostScript are points: = 1/72 inch
*     we use 1/10 mm => scale .283464 (72/254)
      SCALE = 72./254.
 
      IXPAGE = 2100
      IYPAGE = 2960
      ITOP   = 2700
      MARGL = 200
      MARGR = 200
      MARGB = 200
      MARGT = 200
 
      ICSIZE =  30
      ISPACE = 30
 
*     the PostScript preample
 
      CALL DZEHPE(LUOUT)
      CALL DZDPLN(LUOUT,'%%Page: 0 0',-3)
      IX = MARGL
      IY = ITOP
      IPAGE = 0
      NL = 0
      IFIG = 0
      NIND = 0
 
*     loop on lines
 
   30 CONTINUE
      CALL DZDCRD(LUSGML,CTEXT,IRC)
      IF(IRC.NE.0)GOTO 70
*      READ(LUSGML,'(A)',END=70 )CTEXT
      IF(CTEXT(1:1) .NE. '<')GOTO 50
 
*     tab section returns after </TAB>
      IF(CTEXT(1:4).EQ.'<TAB')THEN
         GOTO 30
      ENDIF
 
*     title
 
      IF(CTEXT(1:8).EQ.'<TITLEP>')THEN
         CALL DZDPLN(LUOUT,' /saveobj save def',-3)
         WRITE(CCINT,'(2F8.5,A)')SCALE,SCALE,' scale'
         CALL DZDPLN(LUOUT,CCINT,-3)
         CALL DZDPLN(LUOUT,' H 50 sf',0)
         GOTO 30
      ENDIF
      IF(CTEXT(1:8).EQ.'</TITLEP')THEN
         CALL DZDPLN(LUOUT,' saveobj restore ',-3)
         CALL DZDPLN(LUOUT,' showpage',-2)
         CALL DZENPG(IPAGE)
         IX = MARGL
         IY = ITOP
         GOTO 30
      ENDIF
      IF(CTEXT(1:4).EQ.'<TIT')THEN
         IY = IY-100
         ILC =INDXBC(CTEXT,' ')
         WRITE(CCINT,'(2I5,A)')IXPAGE/2,IY, ' 0 ('
         CALL DZDPLN(LUOUT,CCINT,-3)
         CALL DZDPLN(LUOUT,CTEXT(8:ILC),0)
         CALL DZDPLN(LUOUT,') cs',0)
         GOTO 30
      ENDIF
      IF(CTEXT(1:4).EQ.'<AUT')THEN
         IY = IY-150
         ILC =INDXBC(CTEXT,' ')
         WRITE(CCINT,'(2I5,2A)')IXPAGE/2,IY, ' 0 ('
         CALL DZDPLN(LUOUT,CCINT,-3)
         CALL DZDPLN(LUOUT,CTEXT(8:ILC),0)
         CALL DZDPLN(LUOUT,') cs',0)
         GOTO 30
      ENDIF
 
*     remember page where bank descriptor starts
 
      IF(CTEXT(1:4).EQ.'<LIS')THEN
         ISPAGE = IPAGE
         IF(IY.LT.MARGB+6*ISPACE)THEN
            CALL DZDPLN(LUOUT,' saveobj restore ',-3)
            CALL DZDPLN(LUOUT,'showpage',-2)
            CALL DZENPG(IPAGE)
            IY = ITOP
         ENDIF
         GOTO 30
      ENDIF
*
      IF(CTEXT(1:3).EQ.'<H1')THEN
         ISPAGE = IPAGE
         IF(IY.LT.MARGB+6*ISPACE)THEN
            CALL DZDPLN(LUOUT,' saveobj restore ',-3)
            CALL DZDPLN(LUOUT,'showpage',-2)
            CALL DZENPG(IPAGE)
            IY = ITOP
         ELSE
            IY = IY-40
         ENDIF
         CALL DZDPLN(LUOUT,' H 40 sf',-2)
         WRITE(CCINT,'(2I5,A)')IX,IY,' m'
         CALL DZDPLN(LUOUT,CCINT,-1)
         INN = INDEX(CTEXT,'"')+1
         IF(INN .NE. 0)THEN
            ILN = INN+INDEX(CTEXT(INN:80),'"')-2
            IF(ILN.LT.INN)ILN=INN
            CALL DZDPLN(LUOUT,'(BankId: ',-2)
            CALL DZDPLN(LUOUT,CTEXT(INN:ILN),0)
            CALL DZDPLN(LUOUT,') sh',0)
 
         ELSE
            IFC = INDEX(CTEXT,'>')+1
            ILC = INDXBC(CTEXT,' ')
            IF(ILC.LT.IFC)ILC=IFC
            CALL DZDPLN(LUOUT,'(',-2)
            CALL DZDPLN(LUOUT,CTEXT(IFC:ILC),0)
            CALL DZDPLN(LUOUT,') sh',0)
         ENDIF
         IY = IY-40
         WRITE(CCINT,'(I8)')ICSIZE
         CALL DZDPLN(LUOUT,'C '//CCINT(1:8)//' sf',-3)
         GOTO 30
      ENDIF
*     entry in index
      IF(CTEXT(1:4).EQ.'<I1>')THEN
         NIND = NIND + 1
         CINDEX(NIND) = CTEXT(5:8)
         IPINDX(NIND) = ISPAGE
         IORD(NIND)=NIND
         GOTO 30
      ENDIF
*     figure ID
      IF(CTEXT(1:7).EQ.'<FIG ID')THEN
         GOTO 30
      ENDIF
*     --enter a picture
      IF(CTEXT(1:4).EQ.'<PIC')THEN
         IFIG = IFIG + 1
         CPNAME = CTEXT(15:21)
         CALL DZEPPA(LUPPIC,CPNAME,IXWID,IYWID)
         IF(IXWID.EQ.0)THEN
            WRITE(*,'(A,A)')' PICTURE not found: ',CPNAME
            GOTO 30
         ENDIF
*        does it fit on page
         IYWID = IYWID*10+100
         IF(IYWID .GT. IYPAGE-MARGB)THEN
            WRITE(*,*)' Picture too big ',IXWID,IYWID
            GOTO 30
         ENDIF
         CALL DZDPLN(LUOUT,' ',-99)
         IF(IY-IYWID .LT. MARGB)THEN
            CALL DZDPLN(LUOUT,' saveobj restore ',-3)
            CALL DZDPLN(LUOUT,' showpage',-2)
            CALL DZENPG(IPAGE)
            IY = ITOP
         ENDIF
 
         IYSH = IY - 1450
         IPBOT = IY - IYWID
         IPTOP = IY
         IY = IY - IYWID - 2*ISPACE
         IXOFF = 150
         CALL DZEPSB(MARGL,IPBOT,IXPAGE-MARGR,IPTOP,LUOUT)
         CALL DZDPLN(LUOUT,' /graphobj save def() pop',-2)
*         CALL DZDPLN(LUOUT,' gsave',-2)
         WRITE(CCINT,'(2I8,A)')IXOFF,IYSH,' t'
         CALL DZDPLN(LUOUT,CCINT,-3)
         CALL DZDPLN(LUOUT,' /scale {} def ',-2)
         CALL DZDPLN(LUOUT,' /showpage {} def ',-2)
         CALL DZDPLN(LUOUT,' ',-99)
         CALL DZECPP(LUPPIC,LUOUT)
         CALL DZDPLN(LUOUT,' graphobj restore',-2)
*         CALL DZDPLN(LUOUT,' gr ',-2)
          CALL DZDPLN(LUOUT,' ',-99)
         GOTO 30
      ENDIF
*     figure caption
      IF(CTEXT(1:5).EQ.'<FIGC')THEN
         CTEMP(1:4) = CTEXT(9:12)
         ILCFC = 4
         NLFC = 0
   40    CONTINUE
         CALL DZDCRD(LUSGML,CTEXT,IRC)
         IF(IRC.NE.0)GOTO 70
*         READ(LUSGML,'(A)',END=70 )CTEXT
         IF(CTEXT(1:4).EQ.'</FI')THEN
            CALL DZDPLN(LUOUT,' H 40 sf',-2)
            WRITE(CCINT,'(2I8,A)')MARGL+50,IPBOT+20,' m'
            CALL DZDPLN(LUOUT,CCINT,-1)
            WRITE(CCINT(1:4),'(I4)')IFIG
            CALL DZDPLN(LUOUT,
     &      '(Fig.:'//CCINT(1:4)//'   '//CTEMP(1:4)//') sh',-2)
            CALL DZDPLN(LUOUT,' H 30 sf',-2)
            CALL DZDPLN(LUOUT,' 50 0 rm',-2)
            DO 45 I=5,ILCFC
             IF(CTEMP(I:I).EQ.'(' .OR. CTEMP(I:I).EQ.')')CTEMP(I:I)=' '
45          CONTINUE
            CALL DZDPLN(LUOUT,'('//CTEMP(5:ILCFC)//') sh',0)
            WRITE(CCINT,'(A,I8,A)')'C ',ICSIZE,' sf'
            CALL DZDPLN(LUOUT,CCINT,-3)
            GOTO 30
         ENDIF
         NLFC = NLFC+1
         IF(NLFC .EQ. 1)THEN
            ILCFC = INDXBC(CTEXT,' ')
            IF(ILCFC .GT. LEN(CTEMP)-5)ILCFC = LEN(CTEMP)-5
            CTEMP(5:)=' '//CTEXT(1:ILCFC)
            ILCFC = ILCFC+5
         ENDIF
         GOTO 40
      ENDIF
*     ignore other control lines
      IF(CTEXT(1:1).EQ.'<' .AND. CTEXT(1:3).NE.'<HP')GOTO 30
 
   50 CONTINUE
 
      ILC = INDXBC(CTEXT,' ')
      IF(ILC.LE.0)GOTO 30
      NC = ILC
*      IBO = 0
*      IBC = 0
      IF( INDEX(CTEXT(1:ILC),'(').NE.0
     &.OR.INDEX(CTEXT(1:ILC),')').NE.0)THEN
          ILC=0
          DO 60 I=1,NC
             ILC=ILC+1
             IF(ILC.GT.99)GOTO 61
             IF(CTEXT(I:I).EQ.'(' .OR. CTEXT(I:I).EQ.')')THEN
               CTEMP(ILC:ILC)=BS
               ILC=ILC+1
             ENDIF
             CTEMP(ILC:ILC)=CTEXT(I:I)
60        CONTINUE
61        CONTINUE
      ELSE
          CTEMP(1:ILC)=CTEXT(1:ILC)
      ENDIF
      IF(IY .LE. MARGB)THEN
         IY = ITOP
         CALL DZDPLN(LUOUT,' saveobj restore ',-3)
         CALL DZDPLN(LUOUT,' showpage',-2)
         CALL DZENPG(IPAGE)
         NL = 0
      ENDIF
      IF(CTEXT(1:3).EQ.'<HP')THEN
         CALL DZDPLN(LUOUT,' H 40 sf',-2)
         IFC = INDEX(CTEXT,'>')+1
         ILC1 = INDEXB(CTEXT,'<')-1
         IF (ILC1.NE.0)ILC=ILC1
      ELSE
         IFC=1
      ENDIF
      WRITE(CCINT,'(2I8,A)')IX,IY,' m'
      CALL DZDPLN(LUOUT,CCINT,-1)
      CALL DZDPLN(LUOUT,'(',0)
      CALL DZDPLN(LUOUT,CTEMP(IFC:ILC),0)
      CALL DZDPLN(LUOUT,') sh',0)
      IF(CTEXT(1:3).EQ.'<HP')THEN
         WRITE(CCINT,'(A,I8,A)')'C ',ICSIZE,' sf'
         CALL DZDPLN(LUOUT,CCINT,-1)
      ENDIF
      NL = NL+1
      IY = IY - ISPACE
      GOTO 30
   70 CONTINUE
      IF(NL.GT.0)THEN
         CALL DZDPLN(LUOUT,' saveobj restore ',-3)
         CALL DZDPLN(LUOUT,' showpage',0)
      ENDIF
      WRITE(*,*)NIND, ' banks documented with ',IFIG,' pictures on ',
     + IPAGE,' pages'
      IF(NIND .LE. 0)GOTO 100
 
      DO 80  I=1,NIND-1
         DO 80  K=I+1,NIND
            I1 = IORD(I)
            K1 = IORD(K)
            IF(LLE(CINDEX(I1),CINDEX(K1)))GOTO 80
            IORD(K) = I1
            IORD(I) = K1
   80 CONTINUE
      DO 90  I=1,NIND
         IF(I.EQ.1)GOTO 85
84       K = IORD(I)
         WRITE(CCINT,'(2I8,A)')IX,IY,' m'
         CALL DZDPLN(LUOUT,CCINT,-1)
         CALL DZDPLN(LUOUT,'(',0)
         CALL DZDPLN(LUOUT,CINDEX(K),0)
         CALL DZDPLN(LUOUT,') sh',0)
         WRITE(CCINT,'(2I8,A)')IX+150,IY,' m'
         CALL DZDPLN(LUOUT,CCINT,-1)
         WRITE(CCINT(1:5),'(I5)')IPINDX(K)
         CALL DZDPLN(LUOUT,'('//CCINT(1:5)//') sh',0)
         IY = IY - 50
         IF(MOD(I,50).EQ.0)THEN
            IY = ITOP-100
            IX = IX + 400
         ENDIF
         IF(I.EQ.1)GOTO 90
85       IF(I.EQ.1 .OR. MOD(I,200).EQ.0)THEN
            IF(MOD(I,200).EQ.0)CALL DZDPLN(LUOUT,' showpage',0)
            IPAGE=IPAGE+1
            WRITE(CCINT,'(A,2I5)')'%%Page:',IPAGE,IPAGE
            CALL DZDPLN(LUOUT,CCINT,-3)
            WRITE(CCINT,'(2F8.5,A)')SCALE,SCALE,' scale'
            CALL DZDPLN(LUOUT,CCINT,-3)
            CALL DZDPLN(LUOUT,' [] 0 setdash',-1)
            CALL DZDPLN(LUOUT,' 1 setlinewidth',-1)
            CALL DZDPLN(LUOUT,'H 40 sf',-1)
            WRITE(CCINT,'(2I5,A,A,A)')IXPAGE/2,ITOP+4*ISPACE, ' 0 ('
     +      //'Index) cs'
            CALL DZDPLN(LUOUT,CCINT,-1)
            IY = ITOP - 100
            IX = MARGL
            IF(I.EQ.1)GOTO 84
         ENDIF
   90 CONTINUE
      CALL DZDPLN(LUOUT,' showpage',-2)
      CALL DZDPLN(LUOUT,' ',-99)
  100 CONTINUE
      END
