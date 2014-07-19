*
* $Id: cyear4.F,v 1.2 1996/12/12 13:49:28 cernlib Exp $
*
* $Log: cyear4.F,v $
* Revision 1.2  1996/12/12 13:49:28  cernlib
* Caldat mods from O. Hell to cater for the year 2000
*
* Revision 1.1.1.1  1996/02/15 17:47:51  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      INTEGER  FUNCTION  CYEAR4 (YEAR2)                                         
C                                                                               
CC    "Compute" YYYY from YY                                                    
C                                                                               
C     This algorithm cannot deliver a "correct" result. It aims to              
C     keep the absolute difference between "this year" and CYEAR4 at            
C     less or equal to 50 :                                                     
C                                                                               
C     If "this 2-digit-year" is less than 50                                    
C     then it assumes                                                           
C        "this century"     for YEAR2 <= 50  and                                
C        "previous century" for YEAR2 >  50                                     
C                                                                               
C     If "this 2-digit-year" is larger than 50                                  
C     then it assumes                                                           
C        "next century"     for YEAR2 <= 50  and                                
C        "this century"     for YEAR2 >  50                                     
C                                                                               
C     Examples:                                                                 
C     this year  YEAR2  CYEAR     one century . . . . . next century            
C     yyyy  yy                50................99  00...............50         
C     1936  36    39     1939                       this_y. YEAR2 CYEAR         
C     1936  36    89     1889   YEAR2  CYEAR        this_year                   
C     1996  96    39     2039   this_year           YEAR2   CYEAR               
C     1996  96    89     1989   this_y. YEAR2 CYEAR                             
C                                                                               
C                                                                               
C     This computation is tied to  DATIME and its COMMON /SLATE/ ISL(1)         
C     hoping that this will give the correct century, even in the               
C     year 2000, see remarks at the very top of the CALDAT source.              
C                                                                               
      INTEGER  YEAR2, I, K                                                      
      COMMON /SLATE/ ISL(40)                                                    
C     ISL(1) = ccyy                                                             
C                                                                               
C     This 2-digit-year --> I (yy),  this century --> K (cc00)                  
C                                                                               
      CALL DATIME (K, I)                                                        
C     K = yymmdd, I = hhmm                                                      
C                                                                               
      I = K / 10000                                                             
      K = ISL(1) - I                                                            
C                                                                               
C     Decide on CYEAR4                                                          
C                                                                               
      IF ( I .LE. 50 )  THEN                                                    
         IF ( YEAR2 .LE. 50 )  THEN                                             
            CYEAR4 = K + YEAR2                                                  
         ELSE                                                                   
            CYEAR4 = K - 100 + YEAR2                                            
         ENDIF                                                                  
      ELSE                                                                      
         IF ( YEAR2 .LE. 50 )  THEN                                             
            CYEAR4 = K + 100 + YEAR2                                            
         ELSE                                                                   
            CYEAR4 = K + YEAR2                                                  
         ENDIF                                                                  
      ENDIF                                                                     
C                                                                               
      END                                                                       
