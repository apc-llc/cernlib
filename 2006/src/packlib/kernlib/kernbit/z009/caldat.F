*
* $Id: caldat.F,v 1.3 2000/01/06 11:01:15 mclareni Exp $
*
* $Log: caldat.F,v $
* Revision 1.3  2000/01/06 11:01:15  mclareni
* Y2K corrections from Otto Hell
*
* Revision 1.2  1996/12/12 13:49:25  cernlib
* Caldat mods from O. Hell to cater for the year 2000
*
* Revision 1.1.1.1  1996/02/15 17:47:51  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      SUBROUTINE CALDAT (IINDEX, CHREP, BINREP, RETC)                           
C                                                                               
CC                                                                              
CCC This is the calendar date routine to end all calendar date routines.        
CC                                                                              
C   It takes any calendar date represention and produces from it                
C   simultaneously many other calendar date representaions, plus a few          
C   extra informations.                                                         
C   CALDAT proper is written in IBM /360 Assembler and is reentrant.            
C   This FORTRAN version has been created for portability in the science        
C   community, and is not reentrant.                                            
C   CALDAT proper delivers the IBM /360 machine date for today, with            
C   - hopefully - correct century.                                              
C   This FORTRAN version relies on DATIME and its COMMON/SLATE/ISL(1),          
C   which is documented in the CERNLIB manual as being 19yy, but which          
C   I "may assume to return the correct date", as I am assured by CERN.         
C                                                                               
C                                                                               
C   AUTHOR: O. HELL, DESY, MARCH 1983                                           
C   FORTRAN 77 VERSION:    MAY   1984                                           
C   Changes: Hell, November 1996:                                               
C         *  Correct ouput of BINREP(5) = packed decimal format                 
C         *  Employing COMMON/SLATE/ISL(1), assuming it to return the           
C            correct date rather than 19yy                                      
C         *  improved guessing of 4-digit year from 2-digit year, see           
C            internal routine CYEAR4                                            
C         *  2-digit year in formats DMY9 and DMY8A with leading 0, e.g         
C                22 NOV 06  and  22.11.06  rather than                          
C                22 NOV  6  and  22.11. 6                                       
C                                                                               
C                                                                               
C   EXTERNAL ROUTINE CALLED:                                                    
C       DATIME   FROM KERNLIB, RETURNS TODAY'S DATE                             
C                                                                               
C   INTERNAL ROUTINES AND FUNCTIONS:                                            
C       NAME     R/F    INPUT            OUTPUTS                                
C       CDMON     R     DAY IN YEAR      MONTH AND DAY IN MONTH                 
C       CLEAP     F     YEAR             LEAP = WHETHER LEAPYEAR OR NOT         
C       CYDIY     R     JULIAN DATE      YEAR, LEAP, DAY IN YEAR                
C       CYEAR4    F     YEAR LIKE 84     YEAR LIKE 1984                         
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CCC THE REPRESENTATIONS                                                         
CC                                                                              
C   TWO ARGUMENTS ARE USED FOR THE PASSING OF THE CALENDAR DATES: A             
C   CHARACTER STRING AND AN ARRAY OF FULL WORDS.                                
C   THE VARIOUS REPRESENTATIONS ARE NUMBERED, AND AN INPUT PARAMETER            
C   ('INPUT INDEX') SPECIFIES THE REPRESENTATION CONTAINING THE INPUT           
C   CALENDAR DATE.                                                              
C   AN EXTRA OUTPUT PARAMETER RECEIVES A RETURNCODE.                            
C   SPECIAL CASES: INPUT INDEX = 0  DESIGNATES 'TODAY'.                         
C                  INPUT YEAR YEAR2, RATHER THAN YEAR, IN CERTAIN INPUT         
C                             FIELDS DESIGNATES 'THIS CENTURY'.                 
C                  INPUT INDEX OR INPUT DATA INVALID:                           
C                             OUTPUT CHARACTER STRING WITH ALL '*',             
C                             OUTPUT NUMBERS ALL    X'81818181'                 
C                                                = -2 122 219 135               
C                                                                               
C   ONE OF THE ARRAY ELEMENTS CONTAINS THE JULIAN DATE (= DAYS SINCE            
C   1.JAN. 1 , WITHOUT GREGOR'S PAUSE)  WHICH,  BEING A PURE INTEGER            
C   NUMBER, IS ESPECIALLY WELL SUITED FOR ARITHMETICAL CALCULATIONS.            
C                                                                               
C                                                                               
C  (ANY $ APPEARING IN THE FOLLOWING TEXT DESIGNATES AN AREA TO BE              
C   MODIFIED WHEN NEW REPRSENTATIONS ARE TO BE ADDED. INSTRUCTIONS              
C   FOLLOW AT THE END OF THIS CODE.)                                            
C                                                                               
C                                                                               
C                                                                               
C                                                                               
CC  CALL FROM FORTRAN 77:                                                       
C                                                                               
C $                                                                             
      INTEGER  IINDEX, BINREP(8), RETC                                          
      CHARACTER   CHREP*119                                                     
C     CHARACTER   DMY14*14, DMY11*11, DMY9*9, DMY10*10                          
C     CHARACTER*8 DMY8A, DMY8B, YMD8, MDY8, YDM8                                
C     CHARACTER*6 DMY6,         YMD6, MDY6, YDM6                                
C     CHARACTER   YD5*5, W4*4, W2*2                                             
C                                                                               
C     EQUIVALENCE (CHREP(  1: 14), DMY14), (CHREP( 15: 25), DMY11),             
C    *            (CHREP( 26: 34), DMY9 ), (CHREP( 35: 44), DMY10),             
C    *            (CHREP( 45: 52), DMY8A), (CHREP( 53: 60), DMY8B),             
C    *            (CHREP( 61: 66), DMY6 ), (CHREP( 67: 74), YMD8 ),             
C    *            (CHREP( 75: 80), YMD6 ), (CHREP( 81: 88), MDY8 ),             
C    *            (CHREP( 89: 94), MDY6 ), (CHREP( 95:102), YDM8 ),             
C    *            (CHREP(103:108), YDM6 ), (CHREP(109:113), YD5  ),             
C    *            (CHREP(114:117), W4   ), (CHREP(118:119), W2   )              
C $                                                                             
C     DMY11  = '16 APR 1982'                                                    
C     IINDEX = 2                                                                
C     CALL CALDAT (IINDEX, CHREP, BINREP, RETC)                                 
C                                                                               
C                                                                               
C     EXAMPLE FOR THE NAMES AND THEIR POSSIBLE VALUES                           
C       NAME     VALUE          IINDEX  (INPUT INDEX: VALUE TO MAKE             
C                                        THIS FIELD THE INPUT FIELD)            
C                12345678901234                                                 
C       DMY14    16. APRIL 1982      1                                          
C       DMY11    16 APR 1982         2                                          
C       DMY9     16 APR 82           3                                          
C       DMY10    16. 4.1982          4                                          
C       DMY8A    16. 4.82            5                                          
C       DMY8B    16/04/82            6                                          
C       DMY6     160482              7                                          
C       YMD8     82/04/16            8                                          
C       YMD6     820416              9                                          
C       MDY8     04/16/82           10                                          
C       MDY6     041682             11                                          
C       YDM8     82/16/04           12                                          
C       YDM6     821604             13                                          
C       YD5      82106              14                                          
C       W4       FRI.                                                           
C       W2       FR                                                             
C                12345678901234                                                 
C                                                                               
C                                                                               
C       NAMES OF THE MONTHS AND THE WEEK DAYS STRINGS,                          
C       BOTH SHORT AND LONG:                                                    
C                                                                               
        CHARACTER*3 MONNS (12)                                                  
        CHARACTER*5 MONNL (12)                                                  
        CHARACTER*2 DAYNS (0:6)                                                 
        CHARACTER*4 DAYNL (0:6)                                                 
      INTEGER  LYEAR (2)                                                        
      INTEGER  MONTAB (12,2)                                                    
C                                                                               
C                                                                               
C       NUMBER REPRESENTATIONS                                                  
C                                                                               
C          BINREP-                                                              
C          ELEMENT  CONTENTS            TYPE         EXAMPLE  IINDEX            
C                                                                               
C          1,2,3    D, M, Y             BINARY   16, 4, 1982     101            
C              4    DAY IN THE YEAR     BINARY           106     102            
C              5    00YYDDDC            PACKED DEC   0082106C    103            
C              6    JULIAN DATE         BINARY        723651     104            
C                   = DAYS SINCE 1.JAN. 1 , WITHOUT GREGOR'S PAUSE              
C              7    WEEKDAY, MO = 0, ETC BINARY            4                    
C              8    WEEK IN THE YEAR    BINARY            15                    
C                   WEEK 1 CONTAINS THE 1ST THURSDAY IN THE YEAR.               
C                   (ACCORDING TO DIN = DEUTSCHE INDUSTRIE-NORM)                
C                                                                               
C          ADDITIONAL INPUT SPECIFICATION:                                      
C            3,4    Y, DAY IN YEAR      BINARY     1982, 106     105            
C                                                                               
C                                                                               
C                                                                               
C                                                                               
CC  CALL FROM FORTRAN IV:                                                       
C                                                                               
C $   INTEGER  IINDEX, BINREP(8), RETC                                          
C $   LOGICAL*1 CHREP(119),                                                     
C    *          DMY14(14), DMY11(11), DMY9 ( 9), DMY10(10), DMY8A( 8),          
C    *          DMY8B( 8), DMY6 ( 6), YMD8 ( 8), YMD6 ( 6), MDY8 ( 8),          
C    *          MDY6 ( 6), YDM8 ( 8), YDM6 ( 6), YD5  ( 5), W4   ( 4),          
C $  *          W2   ( 2)                                                       
C     EQUIVALENCE (CHREP(  1), DMY14(1)), (CHREP( 15), DMY11(1)),               
C    *            (CHREP( 26), DMY9 (1)), (CHREP( 35), DMY10(1)),               
C    *            (CHREP( 45), DMY8A(1)), (CHREP( 53), DMY8B(1)),               
C    *            (CHREP( 61), DMY6 (1)), (CHREP( 67), YMD8 (1)),               
C    *            (CHREP( 75), YMD6 (1)), (CHREP( 81), MDY8 (1)),               
C    *            (CHREP( 89), MDY6 (1)), (CHREP( 95), YDM8 (1)),               
C    *            (CHREP(103), YDM6 (1)), (CHREP(109), YD5  (1)),               
C $  *            (CHREP(114), W4   (1)), (CHREP(118), W2   (1))                
C                                                                               
C     DATA DMY11 /'16 APR 1982'/, IINDEX /2/                                    
C     CALL CALDAT (IINDEX, CHREP, BINREP, RETC)                                 
C                                                                               
C                                                                               
C     MEANING OF THE STRINGS AND ARRAY ELEMENTS AS WELL AS NAMES OF THE         
C     MONTHS STRINGS AND EXAMPLES SEE FORTRAN 77                                
C                                                                               
C                                                                               
C                                                                               
CC                                                                              
CCC   MEANING OF RETURN CODE  (RETC, ARGUMENT 4)                                
C                                                                               
C                                                                               
C        RETC   MEANING                                                         
C           0   EVERYTHING FINE                                                 
C           4   IINDEX < 0                                                      
C           8   UPPER BOUND FOR CHREP < IINDEX < LOWER BOUND FOR BINREP         
C          12   UPPER BOUND FOR BINREP < IINDEX                                 
C          16   DDD       OUT OF BOUNDS                                         
C          20   MM | DD   OUT OF BOUNDS                                         
C          24   YEAR      OUT OF BOUNDS                                         
C        1001   SYNTAX ERROR IN DMY14       IINDEX = 1                          
C        1002   SYNTAX ERROR IN DMY11                2                          
C        1003   SYNTAX ERROR IN DMY9                 3                          
C        1004   SYNTAX ERROR IN DMY10                4                          
C        1005   SYNTAX ERROR IN DMY8A                5                          
C        1006   SYNTAX ERROR IN DMY8B                6                          
C        1007   SYNTAX ERROR IN DMY6                 7                          
C        1008   SYNTAX ERROR IN YMD8                 8                          
C        1009   SYNTAX ERROR IN YMD6                 9                          
C        1010   SYNTAX ERROR IN MDY8                10                          
C        1011   SYNTAX ERROR IN MDY6                11                          
C        1012   SYNTAX ERROR IN YDM8                12                          
C        1013   SYNTAX ERROR IN YDM6                13                          
C        1014   SYNTAX ERROR IN YD5                 14                          
C $                                                                             
C        1103   SYNTAX ERROR IN JULIUS             103                          
C        1104   SYNTAX ERROR IN JULIAN             104                          
C $                                                                             
C                                                                               
CC                                                                              
CC                                                                              
CCC   BEGINNING                                                                 
CC                                                                              
CC                                                                              
C                                                                               
C                                                                               
CC    CONSTANTS                                                                 
C                                                                               
C                                                                               
C     For use with DATIME:                                                      
      COMMON /SLATE/ ISL(40)                                                    
C                                                                               
      INTEGER  Z10E6, Z10E5, Z10000, Z1000, Z100, Z10, ZE, ZC, Z81X4            
      PARAMETER ( Z10E6 = 16**6, Z10E5 = 16**5, Z10000 = 16**4,                 
     *            Z1000 = 16**3, Z100 = 16**2,                                  
     *            Z10 = 16, ZE = 14, ZC = 12,                                   
     *            Z81X4 = -2 122 219 135                         )              
C                 -2 122 219 135  =  HEX 81818181                               
C                                                                               
      INTEGER  KINXBL                                                           
      PARAMETER ( KINXBL = 101 )                                                
C $            KINXCH   14     HIGHEST IINDEX FOR CHAR REPRES.                  
C              KINXBL  101     LOWEST  IINDEX FOR BIN REPRES.                   
C $            KINXBH  105     HIGHEST IINDEX FOR BIN REPRES.                   
C                                                                               
C                                                                               
CC    VARIABLES                                                                 
CC                                                                              
C                                                                               
C                                                                               
C                                IMPORTANT LOCAL REGISTERS                      
      INTEGER  LEAP, YEAR, YEAR2, MONTH, DAY, DIY, JULIAN                       
C                                                                               
C         LEAP         1 OR 2    NORMAL YEAR OR LEAP YEAR                       
C         YEAR         E.G. 1984  (and then YEAR2 = 84)                         
C         MONTH        1 .. 12                                                  
C         DAY          1 .. 31   DAY IN MONTH                                   
C         DIY          1 .. 366  DAY IN YEAR                                    
C         JULIAN       > 0       JULIAN DATE = DAYS SINCE 1. JAN. 1             
C                                                                               
C                                                                               
C                                INTERNAL FUNCTIONS                             
      INTEGER  CLEAP, CYEAR4                                                    
C                                                                               
C                                                                               
C                                SHORT RANGE VARIABLES                          
      INTEGER  I, J, K                                                          
C                                                                               
C                                                                               
CC    NORMAL YEAR / LEAP YEAR DATA                                              
C                                                                               
C                                 DAYS IN THE MONTHS                            
      INTEGER  DAYTAB (12,2)                                                    
C                                 DAYS IN THE YEAR                              
C                                 (COPY IN ROUTINE CYDIY)                       
      DATA LYEAR / 365, 366 /                                                   
C                                                                               
C                                 DAYS IN THE MONTHS ACCUMULATED                
C                                 (COPY OF THIS TABLE IN CDMON)                 
      DATA MONTAB /                                                             
     *  000, 031, 059, 090, 120, 151, 181, 212, 243, 273, 304, 334,             
     *  000, 031, 060, 091, 121, 152, 182, 213, 244, 274, 305, 335 /            
C                                                                               
C                                 DAYS IN THE MONTHS                            
      DATA  DAYTAB /                                                            
     *   31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31,             
     *   31,  29,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 /            
C                                                                               
        DATA MONNS /                                                            
     *      'JAN',   'FEB',   'MAR',   'APR',   'MAY',   'JUN',                 
     *      'JUL',   'AUG',   'SEP',   'OCT',   'NOV',   'DEC'       /          
C                                                                               
        DATA MONNL /                                                            
     *      'JAN. ', 'FEB. ', 'MARCH', 'APRIL', 'MAY  ', 'JUNE ',               
     *      'JULY ', 'AUG. ', 'SEPT.', 'OCT. ', 'NOV. ', 'DEC. '     /          
C                                                                               
        DATA DAYNS /                                                            
     *      'MO',   'TU',   'WE',   'TH',   'FR',   'SA',   'SU'     /          
C                                                                               
        DATA DAYNL /                                                            
     *      'MON.', 'TUE.', 'WED.', 'THUR', 'FRI.', 'SAT.', 'SUN.'   /          
C                                                                               
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CCC   IINDEX, CHECKS AND BRANCH TO CORRESPONDING INPUT ROUTINE                  
CC                                                                              
C                                                                               
C     THIS OUGHT TO BE A CASE STATEMENT:                                        
C                                                                               
      IF ( IINDEX .LT. 0 )  GO TO  80004                                        
C                                                                               
      IF ( IINDEX .EQ. 0 )  GO TO  10000                                        
C $                                                                             
      GO TO ( 10010, 10020, 10030, 10040, 10050, 10060, 10070,                  
     *        10080, 10090, 10100, 10110, 10120, 10130, 10140 ), IINDEX         
C                                                                               
C     IINDEX IS GREATER THAN KINXCH.                                            
C                                                                               
      IF ( IINDEX .LT. KINXBL )  GO TO  80008                                   
C $                                                                             
      GO TO ( 11010, 11020, 11030, 11040, 11050 ), IINDEX - 100                 
C                                                                               
C     IINDEX IS GREATER THAN KINXBH.                                            
C                                                                               
      GO TO  80012                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CC    AFTER BRANCHING  WITH IINDEX THE INPUT DATA IS INTERPRETED.               
CC    THEN YEAR, YEAR2, MONTH, DAY, LEAP, DIY, AND JULIAN ARE                   
CC    DETERMINED.                                                               
CC    AFTER THAT, ALL THE OTHER REPRESENTATIONS ARE PRODUCED.                   
CC                                                                              
C                                                                               
CC                                                                              
CC                                                                              
CCC   INPUT DATE = TODAY, IINDEX = 0            'ROUTINE INX0'                  
CC                                                                              
C                                 NO SYNTAX CHECKS NECESSARY                    
10000 CONTINUE                                                                  
      CALL DATIME (K, I)                                                        
C     K = YYMMDD, I = HHMM; TIME NOT USED.                                      
C                                                                               
      YEAR  = ISL (1)                                                           
      I     = K / 100                                                           
      DAY   = K - I * 100                                                       
      YEAR2 = I / 100                                                           
      MONTH = I - YEAR2 * 100                                                   
C                                 YEAR -> LEAP, DIY, JULIAN                     
      GO TO  18230                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX1'                  
CCC   INPUT DATE = '16. APRIL 1982',  FIELD CHREP(  1: 14), IINDEX =  1         
CC          SUBSTR  12345678901234          DMY14                               
C                                 SYNTAX CHECKS                                 
10010 CONTINUE                                                                  
      IF ( CHREP ( 3: 3) .NE. '.'  .OR.                                         
     *     CHREP ( 4: 4) .NE. ' '  .OR.                                         
     *     CHREP (10:10) .NE. ' '       )  GO TO  81001                         
C                                                                               
C     THIS AND ALL FOLLOWING READ STATEMENTS USE IOSTAT=  RATHER THAN           
C     ERR=  BECAUSE THE SIEMENS FORTRAN 77 COMPILER DOESN'T ACCEPT ERR=         
C     IN READ A STATEMENT ON AN INTERNAL FILE.                                  
C     NO PROBLEM WITH THE IBM VS FORTRAN COMPILER.                              
C                                 DECODE DAY                                    
      READ ( CHREP, '(I2)', IOSTAT=K ) DAY                                      
      IF  ( K .NE. 0 )  GO TO  81001                                            
C                                 DECODE MONTH                                  
      DO  10011, MONTH = 1, 12                                                  
         IF  ( CHREP (5:9) .EQ. MONNL (MONTH) )  GO TO  10012                   
10011 CONTINUE                                                                  
C                                 MONTH NOT IDENTIFIED:                         
      GO TO  80020                                                              
10012 CONTINUE                                                                  
C                                 DECODE YEAR                                   
      READ ( CHREP (11:14), '(I4)', IOSTAT=K ) YEAR                             
      IF  ( K .NE. 0 )  GO TO  81001                                            
C                                 LEAP, DIY, JULIAN                             
      YEAR2 = YEAR - (YEAR/100)*100                                             
C                                 YEAR -> LEAP, DIY, JULIAN                     
      GO TO  18230                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX2'                  
CCC   INPUT DATE = '16 APR 1982',     FIELD CHREP( 15: 25), IINDEX =  2         
CC          SUBSTR  56789012345             DMY11                               
C                                 SYNTAX CHECKS                                 
10020 CONTINUE                                                                  
      IF  ( CHREP (17:17) .NE. ' '  .OR.                                        
     *     CHREP (21:21) .NE. ' '        )  GO TO  81002                        
C                                                                               
C                                 DECODE DAY                                    
      READ ( CHREP (15:16), '(I2)', IOSTAT=K ) DAY                              
      IF  ( K .NE. 0 )  GO TO  81002                                            
C                                 DECODE MONTH                                  
      DO  10021, MONTH = 1, 12                                                  
         IF  ( CHREP (18:20) .EQ. MONNS (MONTH) )  GO TO  10022                 
10021 CONTINUE                                                                  
C                                 MONTH NOT IDENTIFIED:                         
      GO TO  80020                                                              
10022 CONTINUE                                                                  
C                                 DECODE YEAR                                   
      READ ( CHREP (22:25), '(I4)', IOSTAT=K ) YEAR                             
      IF  ( K .NE. 0 )  GO TO  81002                                            
C                                 LEAP, DIY, JULIAN                             
      YEAR2 = YEAR - (YEAR/100)*100                                             
C                                 YEAR -> LEAP, DIY, JULIAN                     
      GO TO  18230                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX3'                  
CCC   INPUT DATE = '14 APR 82',       FIELD CHREP( 26: 34), IINDEX =  3         
CC          SUBSTR  678901234               DMY9                                
C                                 SYNTAX CHECKS                                 
10030 CONTINUE                                                                  
      IF  ( CHREP (28:28) .NE. ' '  .OR.                                        
     *     CHREP (32:32) .NE. ' '        )  GO TO  81003                        
C                                                                               
C                                 DECODE DAY                                    
      READ ( CHREP (26:27), '(I2)', IOSTAT=K ) DAY                              
      IF  ( K .NE. 0 )  GO TO  81003                                            
C                                 DECODE MONTH                                  
      DO  10031, MONTH = 1, 12                                                  
         IF  ( CHREP (29:31) .EQ. MONNS (MONTH) )  GO TO  10032                 
10031 CONTINUE                                                                  
C                                 MONTH NOT IDENTIFIED:                         
      GO TO  80020                                                              
10032 CONTINUE                                                                  
C                                 DECODE YEAR                                   
      READ ( CHREP (33:34), '(I2)', IOSTAT=K ) YEAR2                            
      IF  ( K .NE. 0 )  GO TO  81003                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX4'                  
CCC   INPUT DATE = '16. 4.1982',      FIELD CHREP( 35: 44), IINDEX =  4         
CC          SUBSTR  5678901234              DMY10                               
C                                 SYNTAX CHECKS                                 
10040 CONTINUE                                                                  
      IF  ( CHREP (37:37) .NE. '.'  .OR.                                        
     *     CHREP (40:40) .NE. '.'        )  GO TO  81004                        
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
      READ ( CHREP (35:44), '(I2,1X,I2,1X,I4)', IOSTAT=K )                      
     *       DAY, MONTH, YEAR                                                   
      IF  ( K .NE. 0 )  GO TO  81004                                            
      YEAR2 = YEAR - (YEAR/100)*100                                             
C                                 YEAR -> LEAP, DIY, JULIAN                     
      GO TO  18230                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX5'                  
CCC   INPUT DATE = '16. 4.82',        FIELD CHREP( 45: 52), IINDEX =  5         
CC          SUBSTR  56789012                DMY8A                               
C                                 SYNTAX CHECKS                                 
10050 CONTINUE                                                                  
      IF  ( CHREP (47:47) .NE. '.'  .OR.                                        
     *     CHREP (50:50) .NE. '.'        )  GO TO  81005                        
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
      READ ( CHREP (45:52), '(I2,1X,I2,1X,I2)', IOSTAT=K )                      
     *       DAY, MONTH, YEAR2                                                  
      IF  ( K .NE. 0 )  GO TO  81005                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX6'                  
CCC   INPUT DATE = '16/04/82',        FIELD CHREP( 53: 60), IINDEX =  6         
CC          SUBSTR  34567890                DMY8B                               
C                                 SYNTAX CHECKS                                 
10060 CONTINUE                                                                  
      IF  ( CHREP (55:55) .NE. '/'  .OR.                                        
     *     CHREP (58:58) .NE. '/'        )  GO TO  81006                        
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
      READ ( CHREP (53:60), '(I2,1X,I2,1X,I2)', IOSTAT=K )                      
     *       DAY, MONTH, YEAR2                                                  
      IF  ( K .NE. 0 )  GO TO  81006                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX7'                  
CCC   INPUT DATE = '160482',          FIELD CHREP( 61: 66), IINDEX =  7         
CC          SUBSTR  123456                  DMY6                                
C                                 DECODE DAY, MONTH, AND YEAR                   
10070 CONTINUE                                                                  
      READ ( CHREP (61:66), '(I2,I2,I2)', IOSTAT=K )                            
     *       DAY, MONTH, YEAR2                                                  
      IF  ( K .NE. 0 )  GO TO  81007                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX8'                  
CCC   INPUT DATE = '82/04/16',        FIELD CHREP( 67: 74), IINDEX =  8         
CC          SUBSTR  78901234                YMD8                                
C                                 SYNTAX CHECKS                                 
10080 CONTINUE                                                                  
      IF  ( CHREP (69:69) .NE. '/'  .OR.                                        
     *     CHREP (72:72) .NE. '/'        )  GO TO  81008                        
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
      READ ( CHREP (67:74), '(I2,1X,I2,1X,I2)', IOSTAT=K )                      
     *       YEAR2, MONTH, DAY                                                  
      IF  ( K .NE. 0 )  GO TO  81008                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX9'                  
CCC   INPUT DATE = '820416',          FIELD CHREP( 75: 80), IINDEX =  9         
CC          SUBSTR  567890                  YMD6                                
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
10090 CONTINUE                                                                  
      READ ( CHREP (75:80), '(I2,I2,I2)', IOSTAT=K )                            
     *       YEAR2, MONTH, DAY                                                  
      IF  ( K .NE. 0 )  GO TO  81009                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX10'                 
CCC   INPUT DATE = '04/16/82',        FIELD CHREP( 81: 88), IINDEX = 10         
CC          SUBSTR  12345678                MDY8                                
C                                 SYNTAX CHECKS                                 
10100 CONTINUE                                                                  
      IF  ( CHREP (83:83) .NE. '/'  .OR.                                        
     *     CHREP (86:86) .NE. '/'        )  GO TO  81010                        
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
      READ ( CHREP (81:88), '(I2,1X,I2,1X,I2)', IOSTAT=K )                      
     *       MONTH, DAY, YEAR2                                                  
      IF  ( K .NE. 0 )  GO TO  81010                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX11'                 
CCC   INPUT DATE = '041682',          FIELD CHREP( 89: 94), IINDEX = 11         
CC          SUBSTR  901234                  MDY6                                
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
10110 CONTINUE                                                                  
      READ ( CHREP (89:94), '(I2,I2,I2)', IOSTAT=K )                            
     *       MONTH, DAY, YEAR2                                                  
      IF  ( K .NE. 0 )  GO TO  81011                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX12'                 
CCC   INPUT DATE = '82/16/04',        FIELD CHREP( 95:102), IINDEX = 12         
CC          SUBSTR  56789012                YDM8                                
C                                 SYNTAX CHECKS                                 
10120 CONTINUE                                                                  
      IF  ( CHREP ( 97: 97) .NE. '/'  .OR.                                      
     *     CHREP (100:100) .NE. '/'        )  GO TO  81012                      
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
      READ ( CHREP (95:102), '(I2,1X,I2,1X,I2)', IOSTAT=K )                     
     *       YEAR2, DAY, MONTH                                                  
      IF  ( K .NE. 0 )  GO TO  81012                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX13'                 
CCC   INPUT DATE = '821604',          FIELD CHREP(103:108), IINDEX = 13         
CC          SUBSTR  345678                  YDM6                                
C                                                                               
C                                 DECODE DAY, MONTH, AND YEAR                   
10130 CONTINUE                                                                  
      READ ( CHREP (103:108), '(I2,I2,I2)', IOSTAT=K )                          
     *       YEAR2, DAY, MONTH                                                  
      IF  ( K .NE. 0 )  GO TO  81013                                            
C                                 YEAR2 -> YEAR, LEAP, DIY, JULIAN              
      GO TO  18210                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX14'                 
CCC   INPUT DATE = '82106',           FIELD CHREP(109:113), IINDEX = 14         
CC          SUBSTR  90123                   YD5                                 
C                                 DECODE DAY IN YEAR AND YEAR                   
10140 CONTINUE                                                                  
      READ ( CHREP (109:113), '(I2,I3)', IOSTAT=K ) YEAR2, DIY                  
      IF  ( K .NE. 0 )  GO TO  81014                                            
C                                 YY -> YEAR, LEAP, MONTH, DAY, JULIAN          
      IF  ( YEAR2 .LT. 0 )  GO TO  80024                                        
      YEAR  = CYEAR4 ( YEAR2 )                                                  
      LEAP  = CLEAP ( YEAR )                                                    
      IF  ( DIY .LT. 0  .OR.  DIY .GT. LYEAR (LEAP) )  GO TO  80016             
      CALL  CDMON ( LEAP, DIY, MONTH, DAY )                                     
C                                 JULIAN                                        
      GO TO  18300                                                              
C                                                                               
C $                                                                             
CC                                                                              
CC                                              'ROUTINE INX101'                
CCC   INPUT DATE = 16, 4, 1982, ELEMENTS 1-3 OF BINREP,    IINDEX = 101         
CC                                                                              
C                                                                               
11010 CONTINUE                                                                  
      DAY   = BINREP (1)                                                        
      MONTH = BINREP (2)                                                        
      YEAR  = BINREP (3)                                                        
C                                                                               
      IF  ( YEAR .LE. 0 )  GO TO  80024                                         
      YEAR2 = YEAR - (YEAR/100)*100                                             
      LEAP  = CLEAP ( YEAR )                                                    
      IF  ( MONTH .LT. 1  .OR.  MONTH .GT. 12 )  GO TO  80020                   
      IF  ( DAY .LT. 1  .OR.  DAY .GT. DAYTAB(MONTH, LEAP)) GO TO 80020         
      DIY   = MONTAB ( MONTH, LEAP ) + DAY                                      
C                                 JULIAN                                        
      GO TO  18300                                                              
C                                                                               
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX102'                
CCC   INPUT DATE = 106 ,        ELEMENT    4 OF BINREP,    IINDEX = 102         
CC                              YEAR IS TAKEN TO BE THIS YEAR                   
C                                                                               
11020 CONTINUE                                                                  
      DIY   = BINREP (4)                                                        
      CALL DATIME (K, I)                                                        
C     K = YYMMDD, I = HHMM; TIME NOT USED.                                      
C                                                                               
      YEAR  = ISL (1)                                                           
      IF  ( YEAR .LE. 0 )  GO TO  80024                                         
      YEAR2 = K / 10000                                                         
      LEAP  = CLEAP ( YEAR )                                                    
      IF  ( DIY .LT. 0  .OR.  DIY .GT. LYEAR (LEAP) )  GO TO  80016             
      CALL  CDMON ( LEAP, DIY, MONTH, DAY )                                     
C                               JULIAN                                          
      GO TO  18300                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX103'                
CCC   INPUT DATE = JULIUS,      ELEMENT    5 OF BINREP,    IINDEX = 103         
C                               00 YY DD DC, PACKED DECIMAL                     
C                                                                               
C     THIS IS A DATA TYPE  N O T  SUPPORTED BY FORTRAN, BUT IT OCCURS           
C     E.G. IN SMF FROM IBM /370 .                                               
C                                                                               
11030 CONTINUE                                                                  
C     Syntax checks: first two hex digits must be 00  and                       
C                    last hex digit must be C or E .                            
      IF ( .NOT. (                                                              
     *              ( 0 .LT. BINREP (5)  .AND.  BINREP (5) .LT. Z10E6 )         
     *        .AND. ( MOD (BINREP (5), Z10 ) .EQ. ZC  .OR.                      
     *                MOD (BINREP (5), Z10 ) .EQ. ZE      )                     
     *            )                                                             
     *    )  GO TO  81103                                                       
C                                                                               
C                                 YEAR                                          
C                                 K <- 00 00 00 YY, I <- 00 00 DD DC            
      K     = BINREP (5) / Z10000                                               
      I     = BINREP (5) - K * Z10000                                           
      YEAR2 = K / Z10                                                           
      YEAR2 = YEAR2 * 10  +  (K - YEAR2 * Z10)                                  
      YEAR  = CYEAR4 ( YEAR2 )                                                  
C                                                                               
C                                 DIY                                           
C                                 K <- 00 00 00 DD, I <- 00 00 0D DD            
      I     = I / Z10                                                           
      K     = I / Z10                                                           
      DIY   = K / Z10                                                           
      DIY   = DIY * 100  +  (K - DIY * Z10) * 10  +  (I - K * Z10)              
C                                                                               
      IF  ( YEAR .LE. 0 )  GO TO  80024                                         
      LEAP  = CLEAP ( YEAR )                                                    
      IF  ( DIY .LT. 0  .OR.  DIY .GT. LYEAR (LEAP) )  GO TO  80016             
      CALL  CDMON ( LEAP, DIY, MONTH, DAY )                                     
C                               JULIAN                                          
      GO TO  18300                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX104'                
CCC   INPUT DATE = JULIAN,      ELEMENT    6 OF BINREP,    IINDEX = 104         
CC                                                                              
11040 CONTINUE                                                                  
      JULIAN = BINREP (6)                                                       
      IF  ( JULIAN .LT. 1 )  GO TO  81104                                       
      CALL  CYDIY ( JULIAN, YEAR, LEAP, DIY )                                   
      CALL  CDMON ( LEAP, DIY, MONTH, DAY )                                     
      YEAR2 = YEAR - (YEAR/100)*100                                             
C                                 PRODUCE OUTPUT DATA & RETURN                  
      GO TO  20000                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                              'ROUTINE INX105'                
CCC   INPUT DATE = 1982, 106,   ELEMENTS 3-4 OF BINREP,    IINDEX = 105         
CC                                                                              
11050 CONTINUE                                                                  
      YEAR  = BINREP (3)                                                        
      DIY   = BINREP (4)                                                        
      IF  ( YEAR .LE. 0 )  GO TO  80024                                         
      YEAR2 = YEAR - (YEAR/100)*100                                             
      LEAP  = CLEAP ( YEAR )                                                    
      IF  ( DIY .LT. 0  .OR.  DIY .GT. LYEAR (LEAP) )  GO TO  80016             
      CALL  CDMON ( LEAP, DIY, MONTH, DAY )                                     
C                               JULIAN                                          
      GO TO  18300                                                              
C                                                                               
C $                                                                             
CC                                                                              
CC                                                                              
CCC   COMMON SECTION FOR VARIOUS INPUTS                                         
CC                                                                              
C                                                                               
C                                                                               
C     Common section for inputs of type  DAY, MONTH, YEAR2                      
C                                                                               
18210 CONTINUE                                                                  
      IF  ( YEAR2 .LT. 0 )  GO TO  80024                                        
      YEAR  = CYEAR4 ( YEAR2 )                                                  
C                                                                               
C     Common section for inputs of type  DAY, MONTH, YEAR, and YEAR2            
C                                                                               
18230 CONTINUE                                                                  
      LEAP  = CLEAP ( YEAR )                                                    
      IF  ( MONTH .LT. 1  .OR.  MONTH .GT. 12 )  GO TO  80020                   
      IF  ( DAY .LT. 1  .OR.  DAY .GT. DAYTAB(MONTH, LEAP)) GO TO 80020         
      DIY   = MONTAB ( MONTH, LEAP ) + DAY                                      
C                                                                               
C     Common section for inputs of type  .NE. JULIAN                            
C                                                                               
18300 CONTINUE                                                                  
      JULIAN = DIY  +  (YEAR - 1) * 365  +  (YEAR - 1) / 4                      
     *              -  (YEAR - 1) / 100  +  (YEAR - 1) / 400                    
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CCC   PRODUCE OUTPUTS AND RETURN                                                
CC                                                                              
C                                                                               
C                                                                               
CC    STORE REGISTERED OUTPUT DATA                                              
C                                                                               
20000 CONTINUE                                                                  
      BINREP (1) = DAY                                                          
      BINREP (2) = MONTH                                                        
      BINREP (3) = YEAR                                                         
      BINREP (4) = DIY                                                          
C $                                                                             
C $                                                                             
CC    STORE JULIUS     00YYDDDC                                                 
C                                                                               
      K          = DIY / 10                                                     
      I          = K / 10                                                       
      BINREP (5) = ZC  +  (DIY - K*10) * Z10  +  (K - I*10) * Z100  +           
     *             I * Z1000                                                    
      I          = YEAR2 / 10                                                   
      BINREP (5) = BINREP(5)  +  (YEAR2 - I*10) * Z10000  +  I *Z10E5           
C                                                                               
C                                                                               
CC    STORE DAY IN WEEK AND WEEK IN YEAR                                        
C                  WIY = ( DIY - DIW + 9 ) / 7                                  
C                  DEFINITION, ACCORDING TO DIN                                 
C                                   = DEUTSCHES INSTITUT FUER NORMUNG:          
C                      WEEK 1 CONTAINS THE 1ST THURSDAY.                        
C                                                                               
      BINREP (6) = JULIAN                                                       
      J          = MOD (JULIAN-1, 7)                                            
      BINREP (7) = J                                                            
      BINREP (8) = (DIY - J + 9) / 7                                            
C                                                                               
C                                                                               
CC    STORE ALPHA STRINGS                                                       
C                                                                               
C     THE FOLLOWING WRITE STATEMENT IS SPLIT UP INTO THREE, BECAUSE THE         
C     SIEMENS FORTRAN 77 COMPILER CONSIDERS THE FORMAT A CHARACTER              
C     STRING, WHICH AS SUCH MAY NOT EXCEED 255 CHARACTERS.                      
C     NO PROBLEM WITH THE IBM VS FORTRAN COMPILER.  J=DAY IN WEEK               
C                                                                               
      WRITE ( CHREP (  1: 52),                                                  
     1'( I2,   ''. '', A5,   '' '', I4,     I2, '' '', A3, '' '', I4,           
     3   I2,   '' '',  A3,   '' '', I2.2,                                       
     4   I2,   ''.'',  I2,   ''.'', I4,     I2, ''.'', I2, ''.'',I2.2)')        
     1   DAY,      MONNL(MONTH),    YEAR,   DAY,   MONNS(MONTH),  YEAR,         
     3   DAY,      MONNS(MONTH),    YEAR2,                                      
     4   DAY,          MONTH,       YEAR,   DAY,       MONTH,     YEAR2         
C                                                                               
      WRITE ( CHREP ( 53: 94),                                                  
     6'( I2.2, ''/'',  I2.2, ''/'', I2.2,   I2.2,      I2.2,      I2.2,         
     8   I2.2, ''/'',  I2.2, ''/'', I2.2,   I2.2,      I2.2,      I2.2,         
     X   I2.2, ''/'',  I2.2, ''/'', I2.2,   I2.2,      I2.2,     I2.2)')        
     6   DAY,          MONTH,       YEAR2,  DAY,       MONTH,     YEAR2,        
     8   YEAR2,        MONTH,       DAY,    YEAR2,     MONTH,     DAY,          
     X   MONTH,        DAY,         YEAR2,  MONTH,     DAY,       YEAR2         
C                                                                               
      WRITE ( CHREP ( 95:119),                                                  
     2'( I2.2, ''/'',  I2.2, ''/'', I2.2,   I2.2,      I2.2,      I2.2,         
     4   I2.2,         I3.3,        A4,     A2                       )')        
     2   YEAR2,        DAY,         MONTH,  YEAR2,     DAY,       MONTH,        
     4   YEAR2,        DIY,       DAYNL(J), DAYNS(J)                            
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CCC   END                                                                       
CC                                                                              
C                                                                               
      RETC =    0                                                               
      GO TO  90000                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CCC   BAD IINDEX                                                                
CC                                                                              
C               RETCODE                                                         
80004 CONTINUE                                                                  
C                              IINDEX < 0 :                                     
      RETC =    4                                                               
      GO TO  88000                                                              
80008 CONTINUE                                                                  
C                              IINDEX < KINXBL:                                 
      RETC =    8                                                               
      GO TO  88000                                                              
80012 CONTINUE                                                                  
C                              IINDEX > KINXBH:                                 
      RETC =   12                                                               
      GO TO  88000                                                              
C                              DDD OFF BOUNDS:                                  
80016 RETC =   16                                                               
      GO TO  88000                                                              
C                              MM | DD  OFF BOUNDS:                             
80020 RETC =   20                                                               
      GO TO  88000                                                              
C                              YEAR  OFF BOUNDS:                                
80024 RETC =   24                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY14:                           
81001 RETC = 1001                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY11:                           
81002 RETC = 1002                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY9:                            
81003 RETC = 1003                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY10:                           
81004 RETC = 1004                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY8A:                           
81005 RETC = 1005                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY8B:                           
81006 RETC = 1006                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN DMY6:                            
81007 RETC = 1007                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN YMD8:                            
81008 RETC = 1008                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN YMD6:                            
81009 RETC = 1009                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN MDY8:                            
81010 RETC = 1010                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN MDY6:                            
81011 RETC = 1011                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN YDM8:                            
81012 RETC = 1012                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN YDM6:                            
81013 RETC = 1013                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN YD5:                             
81014 RETC = 1014                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN JULIUS:                          
81103 RETC = 1103                                                               
      GO TO  88000                                                              
C                              SYNTAX ERROR IN JULIAN:                          
81104 RETC = 1104                                                               
      GO TO  88000                                                              
C                                                                               
C $                                                                             
C                                                                               
C                              CHREP ALL '*', BINREP ALL HEX 81                 
88000 CONTINUE                                                                  
      DO  88010, I = 1, 119                                                     
         CHREP (I:I) = '*'                                                      
88010 CONTINUE                                                                  
C                                                                               
      DO  88020, I = 1, 8                                                       
         BINREP (I) = Z81X4                                                     
88020 CONTINUE                                                                  
C                                                                               
      GO TO  90000                                                              
C                                                                               
C                                                                               
CC                                                                              
CC                                                                              
CCC   INSTRUCTIONS HOW TO ADD MORE REPRESENTATIONS                              
CC                                                                              
C                                                                               
C     ALL AREAS WHERE CHANGES MAY BE NECESSARY ARE MARKED WITH  $ .             
C     ADDITIONS MAY ME MADE TO THE CHARACTER STRING OR TO THE                   
C     INTEGER ARRAY.                                                            
C     THE FOLLOWING AREAS ARE RELEVANT:                                         
C        - THE CALLS FROM THE VARIOUS LANGUAGES                                 
C             - DECLARATIONS                                                    
C             - SUBFIELD NAMES AND/OR DESCRIPTIONS                              
C        - DESCRIPTION OF THE RETURN CODES                                      
C        - JUMP TABLE TO THE INPUT ROUTINES (LABELS 10XX0 OR 11XX0)             
C        - NEW INPUT ROUTINE, LABEL 10XX0 OR 11XX0 .                            
C        - STORAGE OF RESULTS, DAYS, MONTH, YEAR.                               
C        - ERROR ROUTINE, LABEL 80XXX OR 81XXX                                  
C        - CONSTANTS KINXCH AND KINXBH - ONLY COMMENT                           
C                                                                               
C     END OF MAIN ROUTINE CALDAT                                                
C     'LOCAL' ROUTINES OF CALDAT FOLLOW                                         
C                                                                               
C                                                                               
C                                                                               
C                                                                               
90000 CONTINUE                                                                  
      END                                                                       
