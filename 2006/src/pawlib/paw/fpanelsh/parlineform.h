/*
 * $Id: parlineform.h,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
 *
 * $Log: parlineform.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 08/04/95  23.40.07  by  Gregory Kozlovsky*/
/*-- Author :    Gregory Kozlovsky   08/04/95*/


/*******************************************************************************
       parlineform.h
       This header file is included by parlineform.c

*******************************************************************************/

#ifndef _PARLINEFORM_INCLUDED
#define _PARLINEFORM_INCLUDED


#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include "hmotif/uxxt.h"

#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
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
        Widget  UxparLineForm;
        Widget  UxparLineRightForm;
        Widget  UxparScaleAdjustButton;
        Widget  UxparValueText;
        Widget  UxparFixToggle;
        Widget  UxparLowLimitText;
        Widget  UxparUppLimitText;
        Widget  UxparStepText;
        Widget  UxparEparabText;
        Widget  UxparEminusText;
        Widget  UxparEplusText;
        Widget  UxparGlobccText;
        Widget  UxparLineLeftForm;
        Widget  UxparNumberValueLabel;
        Widget  UxparNameText;
        Widget  UxparScale;
        Widget  UxparBotSeparator;
        Widget  UxparScaleMinMaxForm;
        Widget  UxparScaleMinText;
        Widget  UxparScaleMinLeftArrow;
        Widget  UxparScaleMinRightArrow;
        Widget  UxparMinMaxSeparator;
        Widget  UxparScaleMaxLeftArrow;
        Widget  UxparScaleMaxRightArrow;
        Widget  UxparScaleMaxText;
        Widget  UxparLineMenu;
        Widget  UxparLineMenu_label;
        Widget  UxparLineMenu_sep;
        Widget  UxparLineMenu_store;
        Widget  UxparLineMenu_sep1;
        Widget  UxparLineMenu_lcurr;
        Widget  UxparLineMenu_curr;
        Widget  UxparLineMenu_sep2;
        Widget  UxparLineMenu_lstored;
        Widget  UxparLineMenu_stored;
        Widget  UxparLineMenu_sep3;
        Widget  UxparLineMenu_lstart;
        Widget  UxparLineMenu_start;
        void    *Uxmainctxt;
        double  Uxvscaleunit;
        Boolean UxparFullForm;
        int     UxparScaleStat;
        int     UxparLimitsStat;
        TextStatusInfo  UxparNameStat;
        TextStatusInfo  UxparValueStat;
        TextStatusInfo  UxparLowLimitStat;
        TextStatusInfo  UxparUppLimitStat;
        TextStatusInfo  UxparStepStat;
        TextStatusInfo  UxparScaleMinStat;
        TextStatusInfo  UxparScaleMaxStat;
        int     UxparLineNumber;
        swidget UxUxParent;
        int     Uxipar;
} _UxCparLineForm;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCparLineForm         *UxParLineFormContext;
#define parLineForm             UxParLineFormContext->UxparLineForm
#define parLineRightForm        UxParLineFormContext->UxparLineRightForm
#define parScaleAdjustButton    UxParLineFormContext->UxparScaleAdjustButton
#define parValueText            UxParLineFormContext->UxparValueText
#define parFixToggle            UxParLineFormContext->UxparFixToggle
#define parLowLimitText         UxParLineFormContext->UxparLowLimitText
#define parUppLimitText         UxParLineFormContext->UxparUppLimitText
#define parStepText             UxParLineFormContext->UxparStepText
#define parEparabText           UxParLineFormContext->UxparEparabText
#define parEminusText           UxParLineFormContext->UxparEminusText
#define parEplusText            UxParLineFormContext->UxparEplusText
#define parGlobccText           UxParLineFormContext->UxparGlobccText
#define parLineLeftForm         UxParLineFormContext->UxparLineLeftForm
#define parNumberValueLabel     UxParLineFormContext->UxparNumberValueLabel
#define parNameText             UxParLineFormContext->UxparNameText
#define parScale                UxParLineFormContext->UxparScale
#define parBotSeparator         UxParLineFormContext->UxparBotSeparator
#define parScaleMinMaxForm      UxParLineFormContext->UxparScaleMinMaxForm
#define parScaleMinText         UxParLineFormContext->UxparScaleMinText
#define parScaleMinLeftArrow    UxParLineFormContext->UxparScaleMinLeftArrow
#define parScaleMinRightArrow   UxParLineFormContext->UxparScaleMinRightArrow
#define parMinMaxSeparator      UxParLineFormContext->UxparMinMaxSeparator
#define parScaleMaxLeftArrow    UxParLineFormContext->UxparScaleMaxLeftArrow
#define parScaleMaxRightArrow   UxParLineFormContext->UxparScaleMaxRightArrow
#define parScaleMaxText         UxParLineFormContext->UxparScaleMaxText
#define parLineMenu             UxParLineFormContext->UxparLineMenu
#define parLineMenu_label       UxParLineFormContext->UxparLineMenu_label
#define parLineMenu_sep         UxParLineFormContext->UxparLineMenu_sep
#define parLineMenu_store       UxParLineFormContext->UxparLineMenu_store
#define parLineMenu_sep1        UxParLineFormContext->UxparLineMenu_sep1
#define parLineMenu_lcurr       UxParLineFormContext->UxparLineMenu_lcurr
#define parLineMenu_curr        UxParLineFormContext->UxparLineMenu_curr
#define parLineMenu_sep2        UxParLineFormContext->UxparLineMenu_sep2
#define parLineMenu_lstored     UxParLineFormContext->UxparLineMenu_lstored
#define parLineMenu_stored      UxParLineFormContext->UxparLineMenu_stored
#define parLineMenu_sep3        UxParLineFormContext->UxparLineMenu_sep3
#define parLineMenu_lstart      UxParLineFormContext->UxparLineMenu_lstart
#define parLineMenu_start       UxParLineFormContext->UxparLineMenu_start
#define mainctxt                UxParLineFormContext->Uxmainctxt
#define vscaleunit              UxParLineFormContext->Uxvscaleunit
#define parFullForm             UxParLineFormContext->UxparFullForm
#define parScaleStat            UxParLineFormContext->UxparScaleStat
#define parLimitsStat           UxParLineFormContext->UxparLimitsStat
#define parNameStat             UxParLineFormContext->UxparNameStat
#define parValueStat            UxParLineFormContext->UxparValueStat
#define parLowLimitStat         UxParLineFormContext->UxparLowLimitStat
#define parUppLimitStat         UxParLineFormContext->UxparUppLimitStat
#define parStepStat             UxParLineFormContext->UxparStepStat
#define parScaleMinStat         UxParLineFormContext->UxparScaleMinStat
#define parScaleMaxStat         UxParLineFormContext->UxparScaleMaxStat
#define parLineNumber           UxParLineFormContext->UxparLineNumber
#define UxParent                UxParLineFormContext->UxUxParent
#define ipar                    UxParLineFormContext->Uxipar

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget  create_parLineForm( swidget _UxUxParent, int _Uxipar );

#endif  /* _PARLINEFORM_INCLUDED */
