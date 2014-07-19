/*
 * $Id: test.c,v 1.1 1997/11/28 17:27:06 mclareni Exp $
 *
 * $Log: test.c,v $
 * Revision 1.1  1997/11/28 17:27:06  mclareni
 * Numerous mods and some new routines to get Control-C working reasonably on NT
 *
 */
#include <windows.h>
#include <stdio.h>
#include "CtrlC.h"

// Test program

KUIPJmpBuf mark;              /* Address for long jump to jump to */

//______________________________________________________________________________
void main()
{
   int jmpret;
   int i=0;
  
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)ConsoleSigHandler,
                        TRUE); 
  // sigsetjmp(&buffer,1);
  jmpret = sigsetjmp( &mark, 1 );
   if( jmpret == 0 )
   {
      printf( "Test for invalid operation - \n" );
      printf( "enter two numbers: \n" );
   }
   else
      printf(" --> Ctrl-C was caught  %d \n", jmpret);

  printf(" go ahead %d \n",i);
  i++;
  
  while(1){
//      GenerateConsoleCtrlEvent(CTRL_C_EVENT,0);
  };
   
}

#if 0
BOOL GenerateConsoleCtrlEvent( 
CTRL_C_EVENT  | CTRL_BREAK_EVENT, // signal to generate 
0                                 // process group to get signal 
); 
#endif
