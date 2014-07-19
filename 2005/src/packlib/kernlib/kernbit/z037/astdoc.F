*
* $Id: astdoc.F,v 1.1.1.1 1996/02/15 17:47:52 mclareni Exp $
*
* $Log: astdoc.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:52  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
Copyright (C) 1988 CAJ Mekenkamp. All Rights Reserved.
Carlo Mekenkamp, President Krugerstraat 42, 1975 EH  IJmuiden, Holland
*
* Date:  10-MAR-1988
* The author of this program does not accept any responsibilities for
* damage caused by use or ill-use of this program.
* This program may be used in combination with FIOPAT.MAR
* together with the program GARFIELD which was written by Rob Veenhof.
*
*       PROGRAM DESCRIPTION:
*
*        Control-C interrupt routines
*
*       Routines below:
*                ASTINT - Init Control-C AST Routines
*                ASTXIT - Exit Control-C AST Routines
*                ASTECC - Enable Control-C AST
*                ASTDCC - Disable Control-C AST
*                ASTCCA - Control-C AST Routine
*                ASTCCH - Control-C Condition Handler
*                ASTSCS - Start Critical Section
*                ASTECS - End Critical Section
*
*       AUTHOR:
*
*        C.A.J. Mekenkamp <MEKENKAM@HLERUL5.BITNET>
*
*       CREATION DATE:   10-MAR-1988
*
*       VERSION:  2.04
*
*                       C H A N G E   L O G
*
*       Date     | Name  | Description
*-----------------------------------------------------------------------
* [change_entry]
*
*       Start of the routines
*
