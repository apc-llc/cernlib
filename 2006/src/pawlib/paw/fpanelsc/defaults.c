/*
 * $Id: defaults.c,v 1.1.1.1 1996/03/01 11:39:07 mclareni Exp $
 *
 * $Log: defaults.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:07  mclareni
 * Paw
 *
 */
/*CMZ :  2.07/00 18/04/95  17.36.40  by  O.Couet*/
/*-- Author :    Gregory Kozlovsky   09/04/95*/

/*******************************************************************************
 Default values for resources which require special defaults.
 =====================> Author: G. Kozlovsky, 1994 <===========================
*******************************************************************************/
#include <X11/Intrinsic.h>
#include "fpanelsh/fplocal.h"

SpecResData res_data;

/***************************************************************************
 Set resource values
***************************************************************************/
void set_hfresources()
{
    res_data.textMarginHeight   = 10;
    res_data.textMarginWidth    = 10;
        /***** parameter line default resources *****/
    res_data.parScaleWidth      = 1500;
    res_data.parScaleResolution = 100;
    res_data.parNameColumns     = 10;
    res_data.parValueColumns    = 10;
    res_data.parStepColumns     = 10;
    res_data.parLimitsColumns   = 10;
    res_data.parErrorsColumns   = 10;
    res_data.parScaleMinMaxColumns = 10;
    res_data.parActiveColor     = "Red";
    res_data.parActiveMarginFract = 8;
    res_data.parBotSeparatorHeight = 56;
        /***** the main panel default resources */
    res_data.topLevelWidth      = 480;
    res_data.topLevelHeight     = 360;
    res_data.histoFitTitle      = "Histogram Fit Panel" FPVERSION;
    res_data.vectorFitTitle     = "Vector Fit Panel" FPVERSION;
    res_data.funcAreaOffset     = 100;
    res_data.statAreaOffset     = 100;
    res_data.settingsAreaOffset = 100;
    res_data.parScrolledShadow  = 56;
    res_data.parScrolledSpacing = 0;
    res_data.parScrolledCorrection = 40;
    res_data.vectnameColumns      = 8;
    res_data.histidColumns      = 6;
    res_data.rangeColumns      = 10;
    res_data.optnColumns        = 6;
    res_data.ffuncColumns       = 8;
    res_data.fmodelColumns       = 8;
    res_data.fvalColumns        = 10;
    res_data.fedmColumns        = 10;
    res_data.errordefColumns    = 10;
    res_data.toleranceColumns   = 10;
    res_data.devsColumns        = 10;
         /***** contour panel *****/
    res_data.kuipNameColumns    = 32;
}
