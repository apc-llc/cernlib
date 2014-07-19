/*
 * $Id: cutexpr.h,v 1.2 1996/05/03 13:01:26 cremel Exp $
 *
 * $Log: cutexpr.h,v $
 * Revision 1.2  1996/05/03 13:01:26  cremel
 * cuts editor: add operator '*'
 *
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 13/03/94  15.50.45  by  Fons Rademakers*/
/*-- Author :*/

/*******************************************************************************
       cutExpr.h
       (Generated from interface file cutExpr.ii)
       This header file is included by cutExpr.c

*******************************************************************************/

#ifndef _CUTEXPR_INCLUDED
#define _CUTEXPR_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include "uxxt.h"

#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/Form.h>
#include <Xm/Form.h>

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure.
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef struct
{
        Widget  UxcutExpression;
        Widget  UxactiveCeToggle;
        Widget  UxcountCeLabel;
        Widget  UxnotCeButton;
        Widget  UxbracketCeLabel;
        Widget  Uxexpr1CeText;
        Widget  UxoperatorMenu_p;
        Widget  UxoperatorNULL;
        Widget  UxoperatorEQ;
        Widget  UxoperatorNE;
        Widget  UxoperatorGT;
        Widget  UxoperatorLT;
        Widget  UxoperatorGE;
        Widget  UxoperatorLE;
        Widget  UxoperatorCT;
        Widget  UxoperatorW;
        Widget  UxoperatorMenu;
        Widget  Uxexpr2CeText;
        Widget  UxlogicalMenu_p;
        Widget  UxlogicalNULL;
        Widget  UxlogicalAND;
        Widget  UxlogicalOR;
        Widget  UxlogicalMULT;
        Widget  UxlogicalMenu;
        swidget Uxcut_parent;
} _UxCcutExpr;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCcutExpr             *UxCutExprContext;
#define cutExpression           UxCutExprContext->UxcutExpression
#define activeCeToggle          UxCutExprContext->UxactiveCeToggle
#define countCeLabel            UxCutExprContext->UxcountCeLabel
#define notCeButton             UxCutExprContext->UxnotCeButton
#define bracketCeLabel          UxCutExprContext->UxbracketCeLabel
#define expr1CeText             UxCutExprContext->Uxexpr1CeText
#define operatorMenu_p          UxCutExprContext->UxoperatorMenu_p
#define operatorNULL            UxCutExprContext->UxoperatorNULL
#define operatorEQ              UxCutExprContext->UxoperatorEQ
#define operatorNE              UxCutExprContext->UxoperatorNE
#define operatorGT              UxCutExprContext->UxoperatorGT
#define operatorLT              UxCutExprContext->UxoperatorLT
#define operatorGE              UxCutExprContext->UxoperatorGE
#define operatorLE              UxCutExprContext->UxoperatorLE
#define operatorCT              UxCutExprContext->UxoperatorCT
#define operatorW               UxCutExprContext->UxoperatorW
#define operatorMenu            UxCutExprContext->UxoperatorMenu
#define expr2CeText             UxCutExprContext->Uxexpr2CeText
#define logicalMenu_p           UxCutExprContext->UxlogicalMenu_p
#define logicalNULL             UxCutExprContext->UxlogicalNULL
#define logicalAND              UxCutExprContext->UxlogicalAND
#define logicalOR               UxCutExprContext->UxlogicalOR
#define logicalMULT             UxCutExprContext->UxlogicalMULT
#define logicalMenu             UxCutExprContext->UxlogicalMenu
#define cut_parent              UxCutExprContext->Uxcut_parent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_cutExpr( swidget _Uxcut_parent );

#endif  /* _CUTEXPR_INCLUDED */
