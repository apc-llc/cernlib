*
* $Id: z009t.F,v 1.1 1996/12/12 13:50:36 cernlib Exp $
*
* $Log: z009t.F,v $
* Revision 1.1  1996/12/12 13:50:36  cernlib
* Add test for caldat z009
*
*
      SUBROUTINE Z009T
C      PROGRAM CALDATT                                                           
C                                                                               
C     BATCH PROGRAM FOR TESTING CALDAT                                          
C                                                                               
C       NAMES OF THE MONTHS STRINGS:                                            
C       3 CHARS: 'JAN',   'FEB',   'MAR',   'APR',   'MAY',   'JUN',            
C                'JUL',   'AUG',   'SEP',   'OCT',   'NOV',   'DEC'             
C       5 CHARS: 'JAN. ', 'FEB. ', 'MARCH', 'APRIL', 'MAY  ', 'JUNE ',          
C                'JULY ', 'AUG. ', 'SEPT.', 'OCT. ', 'NOV. ', 'DEC. '           
C                                                                               
C       NAMES OF THE WEEK DAYS:                                                 
C       2 CHARS: 'MO',   'TU',   'WE',   'TH',   'FR',   'SA',   'SU'           
C       4 CHARS: 'MON.', 'TUE.', 'WED.', 'THUR', 'FRI.', 'SAT.','SUN.'          
C                                                                               
C                                                                               
      INTEGER IINDEX, RETC                                                      
C                                                                               
C     CALDAT VARIABLES FOR "TODAY", USED AS REFERENCE                           
C                                                                               
      INTEGER BINREP(8)                                                         
      CHARACTER   CHREP*119                                                     
      CHARACTER   DMY14*14, DMY11*11, DMY9*9, DMY10*10                          
      CHARACTER*8 DMY8A, DMY8B, YMD8, MDY8, YDM8                                
      CHARACTER*6 DMY6,         YMD6, MDY6, YDM6                                
      CHARACTER   YD5*5, W4*4, W2*2                                             
C                                                                               
      EQUIVALENCE (CHREP(  1: 14), DMY14), (CHREP( 15: 25), DMY11),             
     *            (CHREP( 26: 34), DMY9 ), (CHREP( 35: 44), DMY10),             
     *            (CHREP( 45: 52), DMY8A), (CHREP( 53: 60), DMY8B),             
     *            (CHREP( 61: 66), DMY6 ), (CHREP( 67: 74), YMD8 ),             
     *            (CHREP( 75: 80), YMD6 ), (CHREP( 81: 88), MDY8 ),             
     *            (CHREP( 89: 94), MDY6 ), (CHREP( 95:102), YDM8 ),             
     *            (CHREP(103:108), YDM6 ), (CHREP(109:113), YD5  ),             
     *            (CHREP(114:117), W4   ), (CHREP(118:119), W2   )              
C                                                                               
C     CALDAT VARIABLES FOR THE VARIOUS FIELDS                                   
C                                                                               
      INTEGER XINREP(8)                                                         
      CHARACTER   XCHREP*119                                                    
      CHARACTER   XDMY14*14, XDMY11*11, XDMY9*9, XDMY10*10                      
      CHARACTER*8 XDMY8A, XDMY8B, XYMD8, XMDY8, XYDM8                           
      CHARACTER*6 XDMY6,          XYMD6, XMDY6, XYDM6                           
      CHARACTER   XYD5*5, XW4*4, XW2*2                                          
C                                                                               
      EQUIVALENCE (XCHREP(  1: 14), XDMY14), (XCHREP( 15: 25), XDMY11),         
     *            (XCHREP( 26: 34), XDMY9 ), (XCHREP( 35: 44), XDMY10),         
     *            (XCHREP( 45: 52), XDMY8A), (XCHREP( 53: 60), XDMY8B),         
     *            (XCHREP( 61: 66), XDMY6 ), (XCHREP( 67: 74), XYMD8 ),         
     *            (XCHREP( 75: 80), XYMD6 ), (XCHREP( 81: 88), XMDY8 ),         
     *            (XCHREP( 89: 94), XMDY6 ), (XCHREP( 95:102), XYDM8 ),         
     *            (XCHREP(103:108), XYDM6 ), (XCHREP(109:113), XYD5  ),         
     *            (XCHREP(114:117), XW4   ), (XCHREP(118:119), XW2   )          
C                                                                               
C     3 RUNS:                                                                   
C                                                                               
      INTEGER RUN                                                               
C                                                                               
      DO 10000 RUN = 1, 3                                                       
         GO TO (10010, 10020, 10030), RUN                                       
C                                                                               
10010    IINDEX = 0                                                             
         WRITE (6, 10011)                                                       
10011    FORMAT              (' 1. RUN: TODAY')                                 
C                             ================                                  
         GO TO 11000                                                            
C                                                                               
C                                                                               
10020    IINDEX = 101                                                           
         WRITE (6, 10021)                                                       
10021    FORMAT              (' 2. RUN: 10 YEARS AGO')                          
C                             =======================                           
         BINREP (3) = BINREP (3) - 10                                           
         GO TO 11000                                                            
C                                                                               
C                                                                               
10030    IINDEX = 101                                                           
         WRITE (6, 10031)                                                       
10031    FORMAT              (' 3. RUN: 10 YEARS FROM NOW')                     
C                             ============================                      
         BINREP (3) = BINREP (3) + 20                                           
         GO TO 11000                                                            
C                                                                               
11000 CONTINUE                                                                  
C                                                                               
C     INITIALISE                                                                
C                                                                               
      CALL CALDAT (IINDEX, CHREP, BINREP, RETC)                                 
      IF  (RETC .EQ. 0)  THEN                                                   
         WRITE  (6, 65) DMY14, DMY11, DMY9, DMY10, DMY8A, DMY8B, DMY6,          
     *                  YMD8, YMD6, MDY8, MDY6, YDM8, YDM6, YD5, W4, W2         
   65    FORMAT (' REFERENCE <---+----1--->', /                                 
     *           ' DMY14     ', A/                                              
     *           ' DMY11     ', A/                                              
     *           ' DMY9      ', A/                                              
     *           ' DMY10     ', A/                                              
     *           ' DMY8A     ', A/                                              
     *           ' DMY8B     ', A/                                              
     *           ' DMY6      ', A/                                              
     *           ' YMD8      ', A/                                              
     *           ' YMD6      ', A/                                              
     *           ' MDY8      ', A/                                              
     *           ' MDY6      ', A/                                              
     *           ' YDM8      ', A/                                              
     *           ' YDM6      ', A/                                              
     *           ' YD5       ', A/                                              
     *           ' W4        ', A/                                              
     *           ' W2        ', A)                                              
         WRITE  (6, 66) BINREP                                                  
   66    FORMAT (' DAY, MONTH, YEAR ', 3I10/                                    
     *           ' DAY IN THE YEAR  ',  I10/                                    
     *           ' PACKED DECIMAL   X''', Z8, ''''/                             
     *           ' JULIAN           ',  I10/                                    
     *           ' DAY IN THE WEEK  ',  I10/                                    
     *           ' WEEK IN THE YEAR ',  I10)                                    
C                                                                               
      ELSE                                                                      
         WRITE  (6, 67) CHREP, BINREP, RETC                                     
   67    FORMAT (' TODAY FAILED', /,                                            
     *           ' CHREP       ', A119, /,                                      
     *           ' BINREP      ', 4I10, 2X, Z8, 3I10 /,                         
     *           ' ERROR CODE =', I10)                                          
         GO TO 9000                                                             
      END IF                                                                    
C                                                                               
C     LOOP OVER CHOICES OF IINDEX                                               
C                                                                               
      DO 1000 KINDEX = 1, 19                                                    
         XCHREP = ' '                                                           
         DO 100 I = 1, 8                                                        
  100       XINREP (I) = 0                                                      
C                                                                               
C        SET IINDEX AND SELECT CHOICE                                           
C                                                                               
         IF (KINDEX .LE. 14) THEN                                               
            IINDEX = KINDEX                                                     
         ELSE                                                                   
            IINDEX = KINDEX + 86                                                
C              101 =     15 + 86 , ...                                          
         ENDIF                                                                  
C                                                                               
         GO TO (1010, 1020, 1030, 1040, 1050, 1060, 1070,                       
     1          1080, 1090, 1100, 1110, 1120, 1130, 1140,                       
     2          1150, 1160, 1170, 1180, 1190)             KINDEX                
C                                                                               
C                                                                               
 1010    CONTINUE                                                               
         WRITE (6, 1011)                                                        
 1011    FORMAT              (' DMY14 ')                                        
C                            ===========                                        
         XDMY14 = DMY14                                                         
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1020    CONTINUE                                                               
         WRITE (6, 1021)                                                        
 1021    FORMAT              (' DMY11 ')                                        
C                            ===========                                        
         XDMY11 = DMY11                                                         
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
 1030    CONTINUE                                                               
         WRITE (6, 1031)                                                        
 1031    FORMAT              (' DMY9  ')                                        
C                            ===========                                        
         XDMY9  = DMY9                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1040    CONTINUE                                                               
         WRITE (6, 1041)                                                        
 1041    FORMAT              (' DMY10 ')                                        
C                            ===========                                        
         XDMY10 = DMY10                                                         
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1050    CONTINUE                                                               
         WRITE (6, 1051)                                                        
 1051    FORMAT              (' DMY8A ')                                        
C                            ===========                                        
         XDMY8A = DMY8A                                                         
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1060    CONTINUE                                                               
         WRITE (6, 1061)                                                        
 1061    FORMAT              (' DMY8B ')                                        
C                            ===========                                        
         XDMY8B = DMY8B                                                         
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1070    CONTINUE                                                               
         WRITE (6, 1071)                                                        
 1071    FORMAT              (' DMY6  ')                                        
C                            ===========                                        
         XDMY6  = DMY6                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1080    CONTINUE                                                               
         WRITE (6, 1081)                                                        
 1081    FORMAT              (' YMD8  ')                                        
C                            ===========                                        
         XYMD8  = YMD8                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1090    CONTINUE                                                               
         WRITE (6, 1091)                                                        
 1091    FORMAT              (' YMD6  ')                                        
C                            ===========                                        
         XYMD6  = YMD6                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1100    CONTINUE                                                               
         WRITE (6, 1101)                                                        
 1101    FORMAT              (' MDY8  ')                                        
C                            ===========                                        
         XMDY8  = MDY8                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1110    CONTINUE                                                               
         WRITE (6, 1111)                                                        
 1111    FORMAT              (' MDY6  ')                                        
C                            ===========                                        
         XMDY6  = MDY6                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1120    CONTINUE                                                               
         WRITE (6, 1121)                                                        
 1121    FORMAT              (' YDM8  ')                                        
C                            ===========                                        
         XYDM8  = YDM8                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1130    CONTINUE                                                               
         WRITE (6, 1131)                                                        
 1131    FORMAT              (' YDM6  ')                                        
C                            ===========                                        
         XYDM6  = YDM6                                                          
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1140    CONTINUE                                                               
         WRITE (6, 1141)                                                        
 1141    FORMAT              (' YD5   ')                                        
C                            ===========                                        
         XYD5   = YD5                                                           
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1150    CONTINUE                                                               
         WRITE (6, 1151)                                                        
 1151    FORMAT              (' BINREP ( 1, 2, 3) ')                            
C                            =======================                            
         XINREP (1) = BINREP (1)                                                
         XINREP (2) = BINREP (2)                                                
         XINREP (3) = BINREP (3)                                                
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1160    CONTINUE                                                               
         WRITE (6, 1161)                                                        
 1161    FORMAT              (' BINREP ( 4 ) ')                                 
C                            ==================                                 
         IF ( RUN .NE. 1) THEN                                                  
            WRITE (6, 1162)                                                     
 1162       FORMAT (' THIS ENTRY WORKS WITH DDD ONLY, AND ASSUMES',             
     1              ' "THIS YEAR". THEREFORE USELESS FOR THIS RUN')             
            GO TO 1000                                                          
         END IF                                                                 
C                                                                               
         XINREP (4) = BINREP (4)                                                
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1170    CONTINUE                                                               
         WRITE (6, 1171)                                                        
 1171    FORMAT              (' BINREP ( 5 ) ')                                 
C                            ==================                                 
         XINREP (5) = BINREP (5)                                                
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1180    CONTINUE                                                               
         WRITE (6, 1181)                                                        
 1181    FORMAT              (' BINREP ( 6 ) ')                                 
C                            ==================                                 
         XINREP (6) = BINREP (6)                                                
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
         GO TO 1000                                                             
C                                                                               
C                                                                               
 1190    CONTINUE                                                               
         WRITE (6, 1191)                                                        
 1191    FORMAT              (' BINREP ( 3  AND  4 ) ')                         
C                            ==========================                         
         XINREP (3) = BINREP (3)                                                
         XINREP (4) = BINREP (4)                                                
         CALL CALDAT (IINDEX, XCHREP, XINREP, RETC)                             
         IF  (RETC .EQ. 0)  THEN                                                
            IF (XCHREP .EQ. CHREP) THEN                                         
               IF (XINREP(1) .EQ. BINREP(1) .AND.                               
     2             XINREP(2) .EQ. BINREP(2) .AND.                               
     3             XINREP(3) .EQ. BINREP(3) .AND.                               
     4             XINREP(4) .EQ. BINREP(4) .AND.                               
     5             XINREP(5) .EQ. BINREP(5) .AND.                               
     6             XINREP(6) .EQ. BINREP(6) .AND.                               
     7             XINREP(7) .EQ. BINREP(7) .AND.                               
     8             XINREP(8) .EQ. BINREP(8)      ) THEN                         
                  WRITE (6, 9061)                                               
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
               ELSE                                                             
                  WRITE (6, 9062)                                               
                  WRITE (6, 9065) XCHREP, XINREP, RETC                          
               END IF                                                           
            ELSE                                                                
               WRITE (6, 9063)                                                  
               WRITE (6, 9065) XCHREP, XINREP, RETC                             
            END IF                                                              
         ELSE                                                                   
            WRITE (6, 9064)                                                     
            WRITE (6, 9065) XCHREP, XINREP, RETC                                
         END IF                                                                 
C                                                                               
C        END OF ONE CALDAT INPUT FORMAT                                         
C                                                                               
 1000 CONTINUE                                                                  
C                                                                               
C     END OF RUN                                                                
C                                                                               
10000 CONTINUE                                                                  
C                                                                               
 9000 STOP                                                                      
C                                                                               
 9061 FORMAT ('    OKAY')                                                       
 9062 FORMAT ('    BINREP FAULTY')                                              
 9063 FORMAT ('    CHREP  FAULTY')                                              
 9064 FORMAT ('    RETURN CODE > 0 ')                                           
 9065 FORMAT (' CHREP       ', A119, /,                                         
     *        ' BINREP      ', 4I10, 2X, Z8, 3I10 /,                            
     *        ' ERROR CODE =', I10)                                             
      END                                                                       
