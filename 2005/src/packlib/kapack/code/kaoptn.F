*
* $Id: kaoptn.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kaoptn.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAOPTN(LUN,STRING,IRC)
*
*.....SPECIFY OPTIONS
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax050.inc"
#include "kapack/kax060.inc"
*
      CHARACTER STRING*(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINN(LUN)
*
*.....CHECK THE STRING AND FIND THE LOCATION OF THE OPTION AND VALUE
      LOCEQU = INDEX(STRING,'=')
      IF ( LOCEQU .LE. 0 )  GO TO 11
      CALL KAXWRD(STRING(:LOCEQU-1),LOC1,LENOPT,*11)
      CALL KAXWRD(STRING(LOCEQU+1:),LOC3,LENVAL,*11)
      LOC2 = LOC1 + LENOPT - 1
      LOC3 = LOC3 + LOCEQU
      LOC4 = LOC3 + LENVAL - 1
      IF ( LOC2+1 .LT. LOCEQU )  THEN
         CALL KAXWRD(STRING(LOC2+1:LOCEQU-1),LOC,L,*1)
         GO TO 11
      ENDIF
 1    IF ( LOC4+1 .LT. LEN(STRING) )  THEN
         CALL KAXWRD(STRING(LOC4+1:),LOC,L,*2)
         GO TO 11
      ENDIF
*
*.....LUNERR OR LUNLOG OPTION
 2    IF ( STRING(LOC1:LOC2).EQ.'LUNERR' .OR.
     +     STRING(LOC1:LOC2).EQ.'LUNLOG' )  THEN
*
         IF ( LENVAL .GT. 2 )  GO TO 31
*
         NUMBER = 0
         DO  3  I = LOC3, LOC4
            IDIGIT = INDEX('0123456789',STRING(I:I)) - 1
            IF ( IDIGIT .LT. 0 )  GO TO 31
            NUMBER = NUMBER*10 + IDIGIT
 3       CONTINUE
*
         IF ( NUMBER .GT. MAXLUN )  GO TO 31
*
         IF ( STRING(LOC1:LOC2) .EQ. 'LUNERR' )  LUNERR = NUMBER
         IF ( STRING(LOC1:LOC2) .EQ. 'LUNLOG' )  THEN
            IF ( NUMBER .EQ. 0 )  THEN
               WRITE(MSG,100) STRING(LOC1:LOC2),STRING(LOC3:LOC4),LUNKAF
               CALL KAXMSG(LUNLOG,MSG)
            ENDIF
            LUNLOG = NUMBER
         ENDIF
*
*.....MODIFY OR RETURN OPTION
      ELSE IF ( STRING(LOC1:LOC2).EQ.'MODIFY' .OR.
     +          STRING(LOC1:LOC2).EQ.'RETURN' )  THEN
*
         IF ( STRING(LOC3:LOC4).NE.'YES' .AND.
     +        STRING(LOC3:LOC4).NE.'NO' )  GO TO 31
*
         IF ( STRING(LOC1:LOC2) .EQ. 'MODIFY' )
     +                             MODIFY = STRING(LOC3:LOC4).EQ.'YES'
*
         IF ( STRING(LOC1:LOC2) .EQ. 'RETURN' )
     +                             RETURN = STRING(LOC3:LOC4).EQ.'YES'
*
*.....INVALID OPTION
      ELSE
         GO TO 21
      ENDIF
*
*.....STORE THE OPTIONS
      IMOD = 0
      IF ( MODIFY )  IMOD = 1
      IRET = 0
      IF ( RETURN )  IRET = 1
      IOPTNS(LUNKAF) = LUNERR*10000 + LUNLOG*100 + IMOD*10 + IRET
#if defined(CERNLIB_VAX)
*
*.....CLOSE AND RE-OPEN THE FILE IF 'MODIFY' REQUESTED
      IF ( STRING(LOC1:LOC2).EQ.'MODIFY' .AND. LBKS(LUNKAF).NE.0 )  THEN
         CLOSE  ( ERR    = 91,
     +            IOSTAT = IOS,
     +            STATUS = 'KEEP',
     +            UNIT   = LUNKAF )
         IF ( IOS .NE. 0 )  GO TO 91
*
         IF ( MODIFY )  THEN
            OPEN  ( ACCESS = 'DIRECT',
     +              ERR    = 92,
     +              FORM   = 'UNFORMATTED',
     +              IOSTAT = IOS,
     +              RECL   = KAXRCL(LUNKAF),
     +              SHARED,
     +              STATUS = 'UNKNOWN',
     +              UNIT   = LUNKAF )
         ELSE
            OPEN  ( ACCESS = 'DIRECT',
     +              ERR    = 92,
     +              FORM   = 'UNFORMATTED',
     +              IOSTAT = IOS,
     +              READONLY,
     +              SHARED,
     +              STATUS = 'OLD',
     +              UNIT   = LUNKAF )
         ENDIF
         IF ( IOS .NE. 0 )  GO TO 92
      ENDIF
#endif
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      WRITE(MSG,100) STRING(LOC1:LOC2), STRING(LOC3:LOC4), LUNKAF
      CALL KAXMSG(LUNLOG,MSG)
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) STRING
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) STRING(LOC1:LOC2)
      GO TO 99
*
 31   IRC = 3
      WRITE(MSG,131) STRING(LOC3:LOC4), STRING(LOC1:LOC2)
      GO TO 99
#if defined(CERNLIB_VAX)
*
 91   WRITE(MSG,191) LUNKAF, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 92   WRITE(MSG,192) LUNKAF, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
#endif
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA090 KAOPTN: OPTION ',A,'=',A,' SET FOR UNIT ',I3)
 111  FORMAT('CRNKA091 KAOPTN: THE SECOND ARGUMENT, ''',A,''', IS NOT
     +        OF THE FORM ''OPTION=VALUE''')
 121  FORMAT('CRNKA092 KAOPTN: ''',A,''' IS NOT A VALID OPTION')
 131  FORMAT('CRNKA093 KAOPTN: ''',A,''' IS NOT AN ACCEPTABLE VALUE FOR
     +        THE ',A,' OPTION')
#if defined(CERNLIB_VAX)
 191  FORMAT('CRNKA094 KAOPTN: CLOSE ERROR WHILE CHANGING THE READ/WRITE
     +        STATUS OF THE KA-FILE ON UNIT ',I3,', IOSTAT = ',I11)
 192  FORMAT('CRNKA094 KAOPTN: OPEN ERROR WHILE CHANGING THE READ/WRITE
     +        STATUS OF THE KA-FILE ON UNIT ',I3,', IOSTAT = ',I11)
#endif
*
      END
