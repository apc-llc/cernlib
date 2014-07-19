*
* $Id: hnform.F,v 1.3 1996/10/11 13:23:44 cernlib Exp $
*
* $Log: hnform.F,v $
* Revision 1.3  1996/10/11 13:23:44  cernlib
* replace tabs by equivalent spaces
*
* Revision 1.2  1996/09/27 07:30:10  cernlib
* Linux mods in string concatenation calling HBUG
*
* Revision 1.1.1.1  1996/01/16 17:08:00  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          10/11/95  12.04.30  by  Julian Bunn
*-- Author :    Julian Bunn   06/04/95
      SUBROUTINE HNFORM(CHFORM,CHNAME,LDIM,CHTYPE,XLOW,XHIGH)
C----------------------------------------------------------------------
C!  -
C!
C!   Author   :- Julian J. Bunn         3-APR-1995
C!
C!   Inputs:
C!        - CHFORM  : Character string to be started or appended to
C!          CHNAME  : Name of the Fortran variable to be described
C!                    Can include dimensions with placeholders, e.g.
C!                    FRED()  will be translated to FRED([LDIM])
C!                    FRED(*) will be translated to FRED([LDIM])
C!                    FRED(*,NY) will be translated to FRED([LDIM],NY)
C!                    FRED(10,10) will be left as-is
C!                    For each * placeholder, there must be a integer
C!                    value passed in the array LDIM.
C!          LDIM    : Dimension(s) of the variable (if not in CHNAME)
C!                    0 ==> not dimensioned
C!                    Pass an array of integers if there is more than
C!                    one * placeholder in CHNAME
C!          CHTYPE  : Type and range of the variable
C!                    [R,I,U,L,C] [*[4,,32]] [:n]
C!                    ' ' ==> Fortran default will be used
C!          XLOW    : Lower limit on the range of the Fortran variable
C!          XHIGH   : Upper limit on the range of the Fortran variable
C!                    If XHIGH <= XLOW no range will be encoded
C!
C!   Outputs:
C!        -
C!          CHFORM  : New character string with the extra information for
C!                    HBNAME, HBNAMC appended (after a comma if necessary)
C!
C!   Externals required: LENOCC HBUG
C!
C!   Description
C!   ===========
C!
C!   Assists the formation of the CHFORM character string for HBNAME/HBNAMC
C!   E.g. for COMMON /EVENT/ NEVT,TRACK(20)
C!
C!        CHFORM = ' '
C!
C!        CALL HNFORM(CHFORM,'TRACK()',20,'R',0.,0.)
C!           results in CHFORM = 'TRACK(20):R'
C!
C!        CALL HNFORM(CHFORM,'NEVT',0,'I:5',0.,100.)
C!           results in CHFORM = 'TRACK(20):R,NEVT[0.,100.]:I:5'
C!
C!   For a full description, see the HBOOK manual
C?
C!======================================================================
      PARAMETER (LFMAX=15)
      CHARACTER*(*) CHFORM,CHNAME
      CHARACTER*(*) CHTYPE
      CHARACTER*(LFMAX) CUSER
      CHARACTER*100 CTEMP,CTEMP1
      INTEGER LDIM(*)
      LOGICAL ISINT
      REAL XLOW,XHIGH
C
      LFORM = LENOCC(CHFORM)
      LAVAIL = LEN(CHFORM)
      LTYPE = LENOCC(CHTYPE)
C
C CHECK FOR SUFFICIENT SPACE IN CHFORM STRING
C
      IF(LAVAIL.LE.LFORM) THEN
         CALL HBUG('+Insufficient space in CHFORM','HNFORM',0)
         GOTO 999
      ENDIF
C
C CHECK RECOGNIZED CHTYPE
C
      IF(LTYPE.GT.0.AND.INDEX('RIULCriulc',CHTYPE(:1)).EQ.0) THEN
         CTEMP=CHTYPE
         CALL HBUG('+Unrecognised type '//CTEMP(:LTYPE),'HNFORM',0)
         GOTO 999
      ENDIF
C
C CHECK ALLOWED SIZE
C
      IF(LTYPE.GT.1) THEN
         IF(CHTYPE(2:2).NE.'*'.AND.CHTYPE(2:2).NE.':') THEN
            CALL HBUG('+Invalid size descriptor','HNFORM',0)
            GOTO 999
         ENDIF
C
C TAKE CARE OF PACKING SPECS
C
         LTC = INDEX(CHTYPE,':')-1
         IF(LTC.EQ.-1) LTC = LTYPE
         IF(LTC.EQ.3) THEN
            READ(CHTYPE(3:3),'(I1)') NBYTE
         ELSE IF(LTC.EQ.4) THEN
            READ(CHTYPE(3:4),'(I2)') NBYTE
         ELSE
            CTEMP=CHTYPE
            CALL HBUG('+Cannot find byte length '//CTEMP(:LTYPE),
     &                'HNFORM',0)
            GOTO 999
         ENDIF
         IF(MOD(NBYTE,4).NE.0.OR.NBYTE.GT.32) THEN
            CTEMP=CHTYPE
            CALL HBUG('+Byte length is bad in '//CTEMP(:LTYPE),
     &                'HNFORM',0)
            GOTO 999
         ENDIF
         IF((CHTYPE(1:1).NE.'C'.AND.CHTYPE(1:1).NE.'c').AND.
     &      NBYTE.GT.8) THEN
            CALL HBUG('+Only *4 or *8 size is allowed','HNFORM',0)
            GOTO 999
         ENDIF
      ENDIF
C
C APPEND A COMMA IF NOT THE FIRST DESCRIPTOR AND ENOUGH SPACE
C
      IF(LFORM.NE.0) THEN
         CHFORM = CHFORM(:LFORM)//','
         LFORM = LFORM+1
      ENDIF
C
C TAKE THE BLANKS OUT OF CHNAME
C
      LNAME = 0
      DO 4 I=1,LENOCC(CHNAME)
         IF(CHNAME(I:I).NE.' ') THEN
            LNAME = LNAME+1
            CTEMP(LNAME:LNAME) = CHNAME(I:I)
         ENDIF
    4 CONTINUE
C
C DETERMINE IF AN INTEGER (FOR RANGE FORMATTING)
C
      ISINT = .FALSE.
      IF(INDEX(CHTYPE,'U').NE.0.OR.INDEX(CHTYPE,'u').NE.0) THEN
         ISINT = .TRUE.
      ELSE IF(INDEX(CHTYPE,'I').NE.0.OR.INDEX(CHTYPE,'i').NE.0) THEN
         ISINT = .TRUE.
      ELSE
         ISINT = LGE(CTEMP(1:1),'I').AND.LLE(CTEMP(1:1),'N')
         ISINT = ISINT.OR.(LGE(CTEMP(1:1),'i').AND.LLE(CTEMP(1:1),'n'))
      ENDIF
C
C GET THE DIMENSIONALITY
C
      NDIM = 0
      IBRAB = INDEX(CTEMP(:LNAME),'(')
      IBRAE = INDEX(CTEMP(:LNAME),')')
      IF(IBRAB.GT.0) THEN
         IF(IBRAE.EQ.0) THEN
            CALL HBUG('+Missing end bracket in variable '
     &                //CTEMP(:LNAME),'HNFORM',0)
            GOTO 999
         ENDIF
         IF(LFORM+IBRAB.GT.LAVAIL) THEN
            CALL HBUG('+Insufficient space in CHFORM','HNFORM',0)
            GOTO 999
         ENDIF
         CHFORM = CHFORM(:LFORM)//CTEMP(:IBRAB)
         LFORM = LFORM + IBRAB
C
C FIND THE NUMBER OF DIMENSIONS FOR THE VARIABLE
C
         NDIM = 1
         DO 1 IPOS=IBRAB+1,IBRAE-1
            IF(CTEMP(IPOS:IPOS).EQ.',') NDIM = NDIM+1
    1    CONTINUE
         IPOS = IBRAB+1
C
C NOW INTERPRET EACH DIMENSION AS FIXED OR VARIABLE
C
         IDIM = 0
         IDIMG = 0
    2    IDIM = IDIM + 1
         IF(IDIM.LE.NDIM) THEN
            IEND = INDEX(CTEMP(IPOS:LNAME),',') + IPOS - 2
            IF(IEND.EQ.IPOS-2) IEND = IBRAE-1
            IF(LFORM+IEND-IPOS+1.GT.LAVAIL) THEN
                CALL HBUG('+Insufficient space in CHFORM','HNFORM',0)
                GOTO 999
            ENDIF
            IF(CTEMP(IPOS:IEND).EQ.'*'.OR.IPOS.EQ.IEND+1) THEN
C
C THE USER WANTS US TO CALCULATE THE DIMENSION STRING ...
C
               IDIMG = IDIMG + 1
               IVAL = LDIM(IDIMG)
               WRITE(CUSER,'(I15.15)') IVAL
               INZ = 0
    3          INZ = INZ+1
               IF(INZ.LT.LFMAX) THEN
                  IF(CUSER(INZ:INZ).EQ.'0') GOTO 3
               ENDIF
               CHFORM = CHFORM(:LFORM)//CUSER(INZ:LFMAX)//
     &                  CTEMP(IEND+1:IEND+1)
               LFORM = LFORM + LFMAX + 2 - INZ
            ELSE
C
C CHECK WHETHER THE GIVEN DIMENSION IS A VARIABLE OR NUMERIC
C
               IF((LLT(CTEMP(IPOS:IPOS),'0').OR.
     &             LGT(CTEMP(IPOS:IPOS),'9')).AND.IDIM.NE.NDIM) THEN
                  CALL HBUG('+Only the last dimension may vary in '//
     &                CTEMP(:LNAME),'HNFORM',0)
                  GOTO 999
               ENDIF
               CHFORM = CHFORM(:LFORM)//CTEMP(IPOS:IEND+1)
               LFORM = LFORM + IEND - IPOS + 2
            ENDIF
            IPOS = IEND + 2
            GOTO 2
         ENDIF
      ELSE
C
C THE VARIABLE IS NOT DIMENSIONED
C
         IF(LFORM+LNAME.GT.LAVAIL) THEN
            CALL HBUG('+Insufficient space in CHFORM','HNFORM',0)
            GOTO 999
         ENDIF
         CHFORM = CHFORM(:LFORM)//CTEMP(:LNAME)
         LFORM = LFORM+LNAME
      ENDIF
C
      IF(XHIGH.GT.XLOW) THEN
C
C LIMITS HAVE BEEN SPECIFIED
C
         IF(ISINT) THEN
            WRITE(CUSER,'(I15.15)') NINT(XLOW)
            INZ = 0
    6       INZ = INZ+1
            IF(INZ.LT.LFMAX) THEN
               IF(CUSER(INZ:INZ).EQ.'0') GOTO 6
            ENDIF
            CTEMP1 = '['//CUSER(INZ:)//','
            LTEMP1 = LFMAX - INZ + 3
            WRITE(CUSER,'(I15.15)') NINT(XHIGH)
            INZ = 0
    7       INZ = INZ+1
            IF(INZ.LT.LFMAX) THEN
               IF(CUSER(INZ:INZ).EQ.'0') GOTO 7
            ENDIF
            CTEMP1 = CTEMP1(:LTEMP1)//CUSER(INZ:)//']'
            LTEMP1 = LTEMP1 + LFMAX - INZ + 2
         ELSE
            WRITE(CUSER,'(F15.5)') XLOW
            INZ = 0
    8       INZ = INZ+1
            IF(INZ.LT.10) THEN
               IF(CUSER(INZ:INZ).EQ.' ') GOTO 8
            ENDIF
            INU = LFMAX + 1
    9       INU = INU-1
            IF(INU.GT.10) THEN
               IF(CUSER(INU:INU).EQ.'0') GOTO 9
            ENDIF
            CTEMP1 = '['//CUSER(INZ:INU)//','
            LTEMP1 = 3 + INU-INZ
            WRITE(CUSER,'(F15.5)') XHIGH
            INZ = 0
   10       INZ = INZ+1
            IF(INZ.LT.10) THEN
               IF(CUSER(INZ:INZ).EQ.' ') GOTO 10
            ENDIF
            INU = LFMAX + 1
   11       INU = INU-1
            IF(INU.GT.10) THEN
               IF(CUSER(INU:INU).EQ.'0') GOTO 11
            ENDIF
            CTEMP1 = CTEMP1(:LTEMP1)//CUSER(INZ:INU)//']'
            LTEMP1 = LTEMP1 + INU-INZ + 2
         ENDIF
         IF(LFORM+LTEMP1.GT.LAVAIL) THEN
            CALL HBUG('+Insufficient space in CHFORM','HNFORM',0)
            GOTO 999
         ENDIF
         CHFORM = CHFORM(:LFORM)//CTEMP1(:LTEMP1)
         LFORM = LFORM+LTEMP1
      ENDIF
C
C APPEND THE TYPE IF ENOUGH SPACE
C
      IF(CHTYPE.NE.' ') THEN
         IF(LFORM+LTYPE.GT.LAVAIL) THEN
            CALL HBUG('+Insufficient space in CHFORM','HNFORM',0)
            GOTO 999
         ENDIF
         CHFORM = CHFORM(:LFORM)//':'//CHTYPE(:LTYPE)
      ENDIF
 999  CONTINUE
      END
