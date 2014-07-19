*
* $Id: uprog.F,v 1.2 1997/11/28 17:27:09 mclareni Exp $
*
* $Log: uprog.F,v $
* Revision 1.2  1997/11/28 17:27:09  mclareni
* Numerous mods and some new routines to get Control-C working reasonably on NT
*
* Kuip
* Revision 1.1.1.1  1996/03/08 15:33:06  mclareni
* Kuip
*
*  1997/11/28 15:33:06  V.Fine
*
* To check ctrl-C handling:
*
*  - The dimensions of the R and CHR arrays in  SUBROUTINE NUMLET
*    have been increased as follows:
*      REAL R(10000)
*      CHARACTER*1 CHR(10000)
*
*  - kubron/kubrof protections for I/O have been introduced 
*                
*
*CMZ :  1.61/03 07/07/89  16.01.01  by
*-- Author :
      PROGRAM TEST
*
      COMMON/PAWC/PAW(50000)
*
* Initialize ZEBRA and the store /PAWC/
*
      CALL MZEBRA(-3)
      CALL MZPAW(50000,' ')
*
* Initialize KUIP with NWORDS words as minimum division size
*
      NWORDS=15000
      CALL KUINIT(NWORDS)
*
* Create user command structure from definition file
* (the definition routine name TESTD is defined in the CDF with '>N TESTD').
*
      CALL TESTD
*
* Set an appropriate prompt for KUIP
*
      CALL KUEXEC('SET/PROMPT ''RANDOM >''')
*
* Give control to KUIP (allowing 'STYLE G')
*
      CALL KUWHAG
*
* Typing 'QUIT' or 'EXIT' we return here
*
      END

      SUBROUTINE TSEED
      CHARACTER*4 OPTION
      CALL KUGETC(OPTION,NCH)
      CALL KUGETI(ISEED)
      IF (OPTION.EQ.'G') THEN
        CALL RDMOUT(ISEED)
        PRINT *,'Seed = ',ISEED
      ELSE IF (OPTION.EQ.'S') THEN
        CALL RDMIN(ISEED)
      ELSE
        PRINT *,'Illegal option'
      ENDIF
      END

      SUBROUTINE NUMLET
      REAL R(10000)
      CHARACTER*1 CHR(10000)
      CHARACTER*32 CMD
      CHARACTER*20 FORMAT,FMT
      CALL KUGETI(N)
      CALL KUGETS(FORMAT,NCH)
      FMT(1:1)='('
      FMT(2:)=FORMAT
      FMT(NCH+2:NCH+2)=')'
      CALL KUPATL(CMD,NPAR)
      DO 10 I=1,N
        R(I)=RNDM(I)
10    CONTINUE
      IF (CMD.EQ.'NUMBER') THEN
*   I/O operation can not be interupted
        call kubrof
        WRITE (6,FMT) (R(I),I=1,N)
*   Unlock interuption handling
        call kubron
      ELSE IF (CMD.EQ.'LETTER') THEN
        DO 20 I=1,N
          J=65+R(I)*26
          CALL ITOCH(J,CHR(I),*20)
20      CONTINUE
*   I/O operation can not be interupted
        call kubrof
        WRITE (6,FMT) (CHR(I),I=1,N)
*   Unlock interuption handling
        call kubron
      ENDIF
99    CONTINUE
      END

